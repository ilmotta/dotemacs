;;; -*- lexical-binding: t; -*-

(require 'cl-lib)
(require 'cl-macs)
(require 'dbus)
(require 'json)
(require 'map)
(require 'subr-x)
(require 'request nil 'no-error)
(require 'lib-util)
(require 'lib-media)
(require 'dash)
(require 'promise)

(defgroup lib-spotify nil
  "Manage Spotify app via D-Bus and time-shift playlists."
  :group 'my
  :prefix "lib-spotify/")

(defcustom lib-spotify/client-id
  nil
  "Spotify's client ID."
  :type 'string)

(defcustom lib-spotify/secret
  nil
  "Spotify's client ID."
  :type 'string)

(defcustom lib-spotify/record-n-play-sink
  "record-n-play"
  "Sink name used to simultaneously record and play."
  :type 'string)

(defvar lib-spotify/api-token
  (let ((map (make-hash-table)))
    (map-put! map 'retrieved-at nil)
    (map-put! map 'value nil)
    map)
  "Temporary Spotify API token memoized by its expiration time.")

(defvar lib-spotify/record-set-state nil
  "Transition the current FSM.")

(defvar lib-spotify/last-response nil)

(defun lib-spotify/playlist-tracks (playlist)
  "Return all tracks in PLAYLIST.

PLAYLIST is the response object parsed as JSON directly from
Spotify's /v1/playlists/:playlist-id endpoint."
  (seq-map
   (lambda (track-item)
     (let ((track (map-elt track-item 'track)))
       `((added-at . ,(map-elt track-item 'added_at))
         (uri . ,(map-elt track 'uri))
         (disc . ,(map-elt track 'disc_number))
         (track . ,(map-elt track 'track_number))
         (title . ,(replace-regexp-in-string "/" "-" (map-elt track 'name)))
         (album . ,(map-nested-elt track '(album name)))
         (duration-ms . ,(map-elt track 'duration_ms))
         (release-year . ,(car (split-string (map-nested-elt track '(album release_date)) "-")))
         (artist . ,(map-elt (seq-first (map-elt track 'artists)) 'name)))))
   (map-nested-elt playlist '(tracks items))))

(defun lib-spotify/track-file-path (track)
  (format "%s/%s (%s)/%s:%02d - %s.flac"
          (map-elt track 'artist)
          (map-elt track 'album)
          (map-elt track 'release-year)
          (map-elt track 'disc)
          (map-elt track 'track)
          (map-elt track 'title)))

(defun lib-spotify/playing-p ()
  "Return t when the Spotify desktop app is playing."
  (equal "Playing" (lib-spotify/playback-status)))

(defun lib-spotify/play (uri)
  "Play Spotify desktop app at URI."
  (dbus-call-method
   :session
   "org.mpris.MediaPlayer2.spotify"
   "/org/mpris/MediaPlayer2"
   "org.mpris.MediaPlayer2.Player"
   "OpenUri"
   uri))

(defun lib-spotify/stop ()
  "Stop Spotify desktop app."
  (dbus-call-method
   :session
   "org.mpris.MediaPlayer2.spotify"
   "/org/mpris/MediaPlayer2"
   "org.mpris.MediaPlayer2.Player"
   "Stop"))

(defun lib-spotify/playback-status ()
  "Return Spotify desktop app playback status."
  (dbus-get-property
   :session
   "org.mpris.MediaPlayer2.spotify"
   "/org/mpris/MediaPlayer2"
   "org.mpris.MediaPlayer2.Player"
   "PlaybackStatus"))

(defun lib-spotify/api-auth-header ()
  (concat "Basic "
          (base64-encode-string
           (format "%s:%s" lib-spotify/client-id lib-spotify/secret)
           'no-line-break)))

(defun lib-spotify/api-auth-token-header (token-data)
  (let* ((access-token (map-elt token-data 'access_token))
         (token-type (map-elt token-data 'token_type)))
    (format "%s %s" token-type access-token)))

(defun lib-spotify/api-expired-token-p ()
  "Return t when the Spotify API token has expired."
  (let* ((now (float-time))
         (retrieved-at (map-elt lib-spotify/api-token 'retrieved-at))
         (expires-in (map-nested-elt lib-spotify/api-token '(value expires_in))))
    (if (and retrieved-at expires-in)
        (> (- now retrieved-at) (- expires-in 30))
      t)))

(defun lib-spotify/api-authenticate ()
  "Retrieve the API token and call SUCCESS-FN with the auth response.

The token will be cached in `lib-spotify/api-token'."
  (if (lib-spotify/api-expired-token-p)
      (promise-new
       (lambda (resolve _reject)
         (request
           "https://accounts.spotify.com/api/token"
           :type "POST"
           :data "grant_type=client_credentials"
           :headers `(("Content-Type" . "application/x-www-form-urlencoded")
                      ("Accept" . "application/json")
                      ("Authorization" . ,(lib-spotify/api-auth-header)))
           :parser #'json-read
           :success (cl-function
                     (lambda (&key data &allow-other-keys)
                       (map-put! lib-spotify/api-token 'retrieved-at (float-time))
                       (map-put! lib-spotify/api-token 'value data)
                       (funcall resolve data))))))
    (promise-resolve (map-elt lib-spotify/api-token 'value))))

(defun lib-spotify/fetch-playlist (auth playlist-id)
  "Fetch playlist PLAYLIST-ID using AUTH to authenticate."
  (promise-new
   (lambda (resolve _reject)
     (request
       (format "https://api.spotify.com/v1/playlists/%s" playlist-id)
       :type "GET"
       :parser #'json-read
       :headers `(("Accept" . "application/json")
                  ("Content-Type" . "application/json")
                  ("Authorization" . ,(lib-spotify/api-auth-token-header auth)))
       :success (cl-function
                 (lambda (&key data &allow-other-keys)
                   (setq lib-spotify/last-response data)
                   (funcall resolve data)))))))

;; Improvements:
;;
;; - Handle song changes when user change song manually. With the current
;; solution of verifying the song duration it works, but it has the edge case of
;; a track being considered valid just because two or more songs have the
;; expected duration reported by the Spotify API.
;;
;; - Change remaining logic to async. All commands should be async.
(defun lib-spotify/record-playlist (playlist-id &optional directory)
  (let ((ctx (make-hash-table))
        (buffer (get-buffer-create (concat "*spotify-record-" playlist-id))))
    (with-current-buffer buffer
      (read-only-mode -1)
      (erase-buffer))
    (cl-labels
        ((record-loop
           (curr-state event)
           (map-put! ctx 'event event)
           (map-put! ctx 'curr-state curr-state)
           (with-current-buffer buffer
             (insert (format "[INFO] Recording: curr-state='%s' event='%s'.\n" curr-state event)))
           (cond ((and (eq curr-state nil) (eq event 'record/start))
                  (condition-case err
                      (with-current-buffer buffer
                        (cl-assert (not lib-spotify/record-set-state) nil "Already recording")
                        (let ((directory (or directory
                                             (make-temp-file (format "spotify-record-%s-%s-"
                                                                     playlist-id
                                                                     (format-time-string "%Y-%m-%dT%H:%M:%S"))
                                                             'dir))))
                          (cl-assert (file-exists-p directory))
                          (map-put! ctx 'directory
                                    (file-name-as-directory (file-truename directory)))
                          (map-put! ctx 'tmp-file
                                    (concat (map-elt ctx 'directory)
                                            (make-temp-name "record-n-play-") ".flac"))
                          (thread-first (lib-spotify/api-authenticate)
                                        (promise-then
                                         (lambda (auth)
                                           (lib-spotify/fetch-playlist auth playlist-id)))
                                        (promise-then
                                         (lambda (playlist)
                                           (map-put! ctx 'tracks
                                                     (thread-last playlist
                                                                  (lib-spotify/playlist-tracks)
                                                                  (seq-sort-by (lambda (track) (map-elt track 'added-at)) #'equal)
                                                                  (seq-reverse)))
                                           (record-loop 'record/playlist-fetched 'record/create-pulse-audio-sink)))
                                        (promise-catch
                                         (lambda (reason)
                                           (map-put! ctx 'error reason)
                                           (record-loop curr-state 'record/error))))))
                    (error
                     (map-put! ctx 'error err)
                     (record-loop curr-state 'record/error))))

                 ((and (eq curr-state 'record/playlist-fetched)
                       (eq event 'record/create-pulse-audio-sink))
                  (condition-case err
                      (with-current-buffer buffer
                        (if-let* ((track (seq-first (map-elt ctx 'tracks))))
                            (progn
                              (lib-spotify/stop)
                              (map-put! ctx 'process-monitor-sink
                                        (lib-media/flac-monitor-pulse-audio-sink
                                         (map-elt ctx 'tmp-file)
                                         lib-spotify/record-n-play-sink))
                              (record-loop 'record/pulse-audio-sink-created 'record/spotify-play))
                          (record-loop 'record/end nil)))
                    (error
                     (map-put! ctx 'error err)
                     (record-loop curr-state 'record/error))))

                 ((and (eq curr-state 'record/pulse-audio-sink-created)
                       (eq event 'record/spotify-play))
                  (condition-case err
                      (with-current-buffer buffer
                        (let ((track (seq-first (map-elt ctx 'tracks))))
                          (insert (format "[INFO] Recording: uri='%s' title='%s'.\n"
                                          (map-elt track 'uri)
                                          (map-elt track 'title)))
                          (lib-spotify/play (map-elt track 'uri)))
                        (run-with-timer
                         3 nil
                         (lambda ()
                           (record-loop 'record/playing 'record/check-playing))))
                    (error
                     (map-put! ctx 'error err)
                     (record-loop curr-state 'record/error))))

                 ((and (eq curr-state 'record/playing)
                       (eq event 'record/check-playing))
                  (condition-case err
                      (with-current-buffer buffer
                        (if (lib-spotify/playing-p)
                            (run-with-timer 5 nil (lambda () (record-loop curr-state event)))
                          (record-loop 'record/track-stopped 'record/encode)))
                    (error
                     (map-put! ctx 'error err)
                     (record-loop curr-state 'record/error))))

                 ((and (eq curr-state 'record/track-stopped)
                       (eq event 'record/encode))
                  (condition-case err
                      (with-current-buffer buffer
                        (let* ((track (seq-first (map-elt ctx 'tracks)))
                               (tmp-file (map-elt ctx 'tmp-file))
                               (new-file (format "%s%s.encoded.flac"
                                                 (file-name-directory tmp-file)
                                                 (file-name-base tmp-file)) ))
                          (map-put! ctx 'new-file new-file)

                          ;; Simply re-creating the input file with sox will fix
                          ;; playback issues.
                          (insert (format  "[INFO] Encoding: uri='%s'.\n" (map-elt track 'uri)))
                          (let ((cmd (format "sox '%s' '%s'"
                                             (lib-util/shell-escape-single-quote tmp-file)
                                             (lib-util/shell-escape-single-quote new-file))))
                            (call-process-shell-command cmd))
                          (record-loop 'record/encoded 'record/validate)))
                    (error
                     (map-put! ctx 'error err)
                     (record-loop curr-state 'record/error))))

                 ((and (eq curr-state 'record/encoded)
                       (eq event 'record/validate))
                  (condition-case err
                      (with-current-buffer buffer
                        (thread-first (lib-media/async-validate-song-duration
                                       (map-elt ctx 'new-file)
                                       (map-elt (seq-first (map-elt ctx 'tracks))
                                                'duration-ms))
                                      (promise-then
                                       (lambda (_)
                                         (record-loop 'record/valid-track 'record/add-metadata)))
                                      (promise-catch
                                       (lambda (reason)
                                         (map-put! ctx 'invalid-track-p t)
                                         (let ((new-file (map-elt ctx 'new-file)))
                                           (with-current-buffer buffer
                                             (insert (format "[ERROR] Track '%s' is probably incomplete. Reason: '%s'\n"
                                                             (map-elt (seq-first (map-elt ctx 'tracks)) 'title)
                                                             (or reason "no further details")))))
                                         (record-loop 'record/valid-track 'record/add-metadata)))))
                    (error
                     (map-put! ctx 'error err)
                     (record-loop curr-state 'record/error))))

                 ((and (eq curr-state 'record/valid-track)
                       (eq event 'record/add-metadata))
                  (condition-case err
                      (with-current-buffer buffer
                        (let* ((track (seq-first (map-elt ctx 'tracks)))
                               (new-file (map-elt ctx 'new-file))
                               (final-file (if (map-elt ctx 'invalid-track-p)
                                               (concat (map-elt ctx 'directory)
                                                       "_REVIEW_/"
                                                       (lib-spotify/track-file-path track))
                                             (concat (map-elt ctx 'directory)
                                                     (lib-spotify/track-file-path track)))))
                          (lib-media/flac-set-basic-metadata new-file track)
                          (make-directory (file-name-directory final-file) 'parents)
                          (rename-file new-file final-file 'overwrite)
                          (record-loop 'record/track-processed 'record/continue)))
                    (error
                     (map-put! ctx 'error err)
                     (record-loop curr-state 'record/error))))

                 ((and (eq curr-state 'record/track-processed)
                       (eq event 'record/continue))
                  (condition-case err
                      (with-current-buffer buffer
                        (when-let* ((process (map-elt ctx 'process-monitor-sink)))
                          (kill-process process))
                        (delete-file (map-elt ctx 'tmp-file))
                        (map-put! ctx 'invalid-track-p nil)
                        (map-put! ctx 'tracks (seq-rest (map-elt ctx 'tracks)))
                        (record-loop 'record/playlist-fetched 'record/create-pulse-audio-sink))
                    (error
                     (map-put! ctx 'error err)
                     (record-loop curr-state 'record/error))))

                 ((eq curr-state 'record/end)
                  (with-current-buffer buffer
                    (insert "Finished processing tracks.\n")
                    (read-only-mode +1)))

                 ((eq event 'record/error)
                  (with-current-buffer buffer
                    (insert (format "[ERROR] Error recording playlist: '%s'" (map-elt ctx 'error)))
                    (when-let* ((process (map-elt ctx 'process-monitor-sink)))
                      (kill-process process))
                    (delete-file (map-elt ctx 'new-file))
                    (delete-file (map-elt ctx 'tmp-file))
                    (ignore-errors (lib-spotify/stop))
                    (record-loop 'record/end nil))))))
      #'record-loop)))

;; (libmedia/pulse-audio-create-sink lib-spotify/record-n-play-sink)
;; (lib-spotify/stop-recording)

;; Playlist 01
;; (let ((fsm (lib-spotify/record-playlist "51rArffVDUcZfUbwSkGH6o")))
;;   (funcall fsm nil 'record/start))

;; Playlist 02
;; (let ((fsm (lib-spotify/record-playlist "1sXNuVKx1i9nLdP8UbbCMW")))
;;   (funcall fsm nil 'record/start))

;; Playlist temp 03
;; (let ((fsm (lib-spotify/record-playlist "~/data/media/temp" "6xJzfOV6uaXsN7EvSXeotC")))
;;   (funcall fsm nil 'record/start))

;; (lib-spotify/playlist-tracks lib-spotify/last-response)

(provide 'lib-spotify)
