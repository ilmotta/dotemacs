;;; -*- lexical-binding: t; -*-

;;; Code:

(defgroup lib-media nil
  "Audio and video utility functions."
  :group 'my
  :prefix "lib/")

(defcustom lib/valid-song-duration-threshold 5000
  "Duration threshold in ms to consider a recording valid."
  :type 'integer)

(defcustom lib/extensions
  '(".flac"
    ".mkv"
    ".mp3"
    ".mp4"
    ".ogg"
    ".wav"
    ".webm")
  "Recognizable media file extensions."
  :type 'list)

(defcustom lib/metadata-regex
  (rx line-start
      (one-or-more not-newline)
      "/" (group (one-or-more not-newline))   ; Artist
      "/" (group (one-or-more not-newline))   ; Album
      "(" (group (= 4 digit)) ")"             ; Year
      "/" (group (= 1 digit))                 ; Disc number
      ":" (group (= 2 digit))                 ; Track number
      " - " (group (one-or-more not-newline)) ; Title
      (regex (string-join lib/extensions "\\|"))
      line-end)
  "Pattern used to extract metadata from absolute file paths."
  :type 'string)

(defcustom lib/flac-metaflac-buffer
  "*metaflac-basic-metadata*"
  "Buffer to insert output from metaflac calls."
  :type 'string)

(defcustom lib/flac-conversion-buffer
  "*extract-flac*"
  "Buffer to write output from ffmpeg FLAC extraction."
  :type 'string)

(defcustom lib/flac-cmd-ffmpeg-extract
  "ffmpeg -y -loglevel repeat+level+warning -i '%s' -vn -codec:a flac '%s'"
  "Command used to extract audio into a new FLAC file.

It should have two string template slots, in order: input
file, output file."
  :type 'string)

(defcustom lib/flac-cmd-ffmpeg-slice
  (concat "ffmpeg -y -loglevel repeat+level+warning "
          "-i '%s' " ; Input file.
          "-ss %s "  ; Beginning of slice.
          "-to %s "  ; End (inclusive) of the slice.
          "'%s'"     ; Output file.
          )
  "Command used to slice an audio file.

Use single quotes around the input and output options. The '-ss'
and '-to' options expect a time duration, for example as
'HH:MM:SS'. See the ffmpeg-utils manual for more details."
  :type 'string)

(defcustom lib/flac-cmd-write-from-stream
  (concat "flac --verify "
          "--compression-level-5 "
          "--endian=little "
          "--sign=signed "
          "--sample-rate=44100 "
          "--bps=16 "
          "--channels=2 "
          "--output-name='%s' "
          "-")
  "Command used to encode PulseAudio Recorder (parec) stream."
  :type 'string)

(defcustom lib/flac-recognizable-input-extensions
  (rx line-start
      (one-or-more not-newline)
      (regex (string-join lib/extensions "\\|"))
      line-end)
  "Recognizable input file extensions when converting to FLAC."
  :type 'string)

(defun lib/pulse-audio-current-sink ()
  "Return current PulseAudio sink name or nil if it fails."
  (let* ((cmd (concat "pacmd list-sinks"
                      "| grep -A1 '* index'"
                      "| grep -oP '<\\K[^ >]+'")))
    (s-presence (s-trim (shell-command-to-string cmd)))))

(defun lib/pulse-audio-sink-exists-p (sink-name)
  "Return t when PulseAudio SINK-NAME already exists."
  (let ((cmd (concat "pacmd list-sinks"
                     (format "| grep 'name: <%s>'" sink-name)
                     "| grep -oP '<\\K[^ >]+'")))
    (equal sink-name (s-trim (shell-command-to-string cmd)))))

;;;###autoload
(defun lib/pulse-audio-create-sink (sink-name)
  "Create sink SINK-NAME if it does not exist.

The new sink is not persisted and if you terminate the PulseAudio
server you'll need to call this function again, for example when
you restart or call 'pactl exit'.

Returns the process."
  (unless (lib/pulse-audio-sink-exists-p sink-name)
    (let ((cmd (concat "pactl load-module "
                       "module-combine-sink "
                       (format "sink_name='%s' " sink-name)
                       (format "slaves='%s' " (lib/pulse-audio-current-sink))
                       (format "sink_properties=device.description='%s'" sink-name))))
      (start-process-shell-command "pactl" nil cmd))))

(defun lib/timestamp->seconds (timestamp)
  "Convert HH:MM:SS timestamp to seconds number."
  (seq-let (h min s) (seq-map #'string-to-number (s-split ":" timestamp))
    (+ (* h 3600) (* 60 min) s)))

(defun lib/infer-basic-metadata (file)
  "Infer metadata from file pattern `--media-metadata-regex'."
  (let ((file (file-truename file)))
    (when (string-match lib/metadata-regex file)
      `((artist . ,(string-trim (match-string 1 file)))
        (album . ,(string-trim (match-string 2 file)))
        (release-year . ,(match-string 3 file))
        (disc . ,(match-string 4 file))
        (track . ,(match-string 5 file))
        (title . ,(file-name-base (string-trim (match-string 6 file))))))))

(defun lib/fd-extensions ()
  "Return valid file extensions for the fd program."
  (->> lib/extensions
       (-map (lambda (ext) (format "--extension '%s'" ext) ))
       (s-join " ")))

(defun lib/directory-find (dir)
  "Return all recognizable media files in DIR."
  (cl-assert (executable-find "fd"))
  (let* ((cmd (format "fd --follow --ignore-case --type f %s" (lib/fd-extensions))))
    (with-temp-buffer
      (let ((default-directory (file-name-as-directory (file-truename dir))))
        (call-process-shell-command cmd nil (current-buffer))
        (->> (s-split "\n" (buffer-string))
             (-filter #'s-present-p)
             (-map (lambda (file) (file-truename file))))))))

(defun lib/async-validate-song-duration (file expected-duration-ms)
  "Async verify FILE is within +-2ms of EXPECTED-DURATION-MS."
  (let* ((file (file-truename file))
         (cmd (format "soxi -D '%s'" (lib-util/shell-escape-single-quote file))))
    (promise-new
     (lambda (resolve reject)
       (thread-first (promise:make-shell-command cmd)
                     (promise-then (lambda (output)
                                     (let ((actual-duration-ms (* 1000 (string-to-number output))))
                                       (message "File '%s' has a duration of %sms." file actual-duration-ms)
                                       (if (< (abs (- actual-duration-ms expected-duration-ms))
                                              lib/valid-song-duration-threshold)
                                           (funcall resolve t)
                                         (funcall reject nil))))))))))

(defun lib/flac-set-basic-metadata (file tags)
  "Remove existing tags from FILE and set new ones using TAGS.

TAGS should be an alist with 'title', 'artist' and 'album' keys.
The command's result is inserted in the
'*metaflac-basic-metadata*' buffer."
  (let ((file (file-truename file)))
    (when tags
      (let ((cmd (concat "metaflac --remove-all-tags "
                         (format "--set-tag='TITLE=%s' "
                                 (lib-util/shell-escape-single-quote (map-elt tags 'title)))
                         (format "--set-tag='ARTIST=%s' "
                                 (lib-util/shell-escape-single-quote (map-elt tags 'artist)))
                         (format "--set-tag='ALBUM=%s' "
                                 (lib-util/shell-escape-single-quote (map-elt tags 'album)))
                         (format "--set-tag='DATE=%s' "
                                 (map-elt tags 'release-year))
                         (format "--set-tag='TRACKNUMBER=%s' "
                                 (map-elt tags 'track))
                         (format "--set-tag='DISCNUMBER=%s' "
                                 (map-elt tags 'disc))
                         (format "'%s'" (lib-util/shell-escape-single-quote file)))))
        (call-process-shell-command cmd nil lib/flac-metaflac-buffer)))))

;;;###autoload
(defun lib/flac-extract-audio (input)
  "Extract audio from INPUT and convert it to FLAC.

INPUT can be a file or directory. In the case of a directory,
files that match a regex will be created in the same directory
and with the same name, but with the .flac extension."
  (interactive "fConvert file or directory to FLAC: ")
  (let* ((input (file-truename input))
         (buffer (generate-new-buffer-name lib/flac-conversion-buffer))
         (files (if (file-directory-p input) (directory-files input) (list input))))
    (thread-last files
                 (seq-filter
                  (lambda (f)
                    (string-match-p lib/flac-recognizable-input-extensions f)))
                 (seq-map
                  (lambda (f)
                    (list (lib-util/shell-escape-single-quote (concat (file-name-directory input) f))
                          (lib-util/shell-escape-single-quote (concat (file-name-directory f)
                                                                  (file-name-base f)
                                                                  ".flac")))))
                 (seq-do
                  (pcase-lambda (`((,input ,output)))
                      (call-process-shell-command
                       (format lib/flac-cmd-ffmpeg-extract input output) nil buffer))))))

;;;###autoload
(defun lib/flac-set-basic-metadata-recursively (dir)
  "Recursively add basic tags inferred from DIR path.

Every file should follow the pattern:

  '%Artist/%Album (%Year{dddd})/%DiscNumber{d}:%TrackNumber{dd} - %Title.flac'

Where {dddd} means 4 consecutive digits.

Metadata will be added using the 'metaflac' program. All existing
tags will be removed."
  (interactive "DDirectory: ")
  (cl-assert (and dir (file-exists-p dir )))
  (cl-assert (executable-find "metaflac"))
  (seq-doseq (file (lib/directory-find dir))
    (lib/flac-set-basic-metadata file (lib/infer-basic-metadata file))))

;;;###autoload
(defun lib/flac-slice (input tracks)
  "Extract audio files from INPUT specified by TRACKS.

Each track in TRACKS must have a 'beg-ms' element that has the
correct time in milliseconds the song starts in the INPUT file,
i.e. considering all previous tracks."
  (cl-assert (and input (file-exists-p input))
             'show-args "Arg INPUT does not exist")
  (let* ((input (file-truename input))
         (dir (file-name-directory input))
         (default-directory dir)
         (buffer (generate-new-buffer-name "*flac-slice*")))
    (seq-doseq (track tracks)
      (let* ((album-dir (expand-file-name (format "%s (%s)"
                                                  (map-elt track 'album)
                                                  (map-elt track 'release-year))))
             (new-file (format "%s/%s:%02d - %s.flac"
                               album-dir
                               (map-elt track 'disc)
                               (map-elt track 'track)
                               (map-elt track 'title)))
             (cmd (format lib/flac-cmd-ffmpeg-slice
                          input
                          (map-elt track 'beg)
                          (map-elt track 'end)
                          (lib-util/shell-escape-single-quote new-file))))
        (make-directory album-dir 'parents)
        (call-process-shell-command cmd nil buffer)))))

;;;###autoload
(defun lib/flac-monitor-pulse-audio-sink (file sink-name)
  "Monitor PulseAudio sink SINK-NAME output in FLAC FILE.

Before you can successfully record audio, remember to open the
PulseAudio Volume Control application and change the application
output device.

Kill the process 'record-n-play' manually if you don't want to
monitor the sink output anymore, for example by using the
`list-processes' buffer."
  (interactive "FPath to record FLAC file: ")
  (let* ((file (file-truename file))
         (dir (file-name-directory file)))
    (cl-assert (file-exists-p dir) nil
               (format "Directory '%s' does not exist" dir))
    (when (lib/pulse-audio-sink-exists-p sink-name)
      (let ((cmd (concat "parec --format=s16le "
                         (format "--device='%s.monitor' | " sink-name)
                         (format lib/flac-cmd-write-from-stream
                                 (lib-util/shell-escape-single-quote file)))))
        (start-process-shell-command "record-n-play" nil cmd)))))

;;;###autoload
(defun lib/generate-video-thumbnail (input)
  "Create thumbnail from INPUT video file."
  (interactive "fInput video file: ")
  (cl-assert (file-exists-p input))
  (let* ((input (file-truename input))
         (dir (file-name-directory input))
         (new-file-name (concat (file-name-base input) ".jpg"))
         (output (concat dir new-file-name))
         (cmd (format "ffmpeg -y -ss 00:00:10.000 -i '%s' -vframes 1 -q:v 2 '%s'" input output)))
    (start-process-shell-command "generate-thumbnail" nil cmd)))

(defconst lib/ffmpeg-video-resolutions
  '("960x720        (4:3)  Standard HD"
    "1280x720      (16:9)  Standard HD"
    "1920x1080     (16:9)  Full HD"
    "1440x1080      (4:3)  Full HD"
    "2560x1080     (21:9)  Ultrawide HD"
    "2560x1440     (16:9)"
    "2304x1440    (16:10)"
    "2160x1440      (3:2)"
    "1920x1440      (4:3)"
    "3840x2400    (16:10)  4K - WQUXGA"
    "3840x2160     (16:9)  4K - UHD"
    "4096x3072      (4:3)  4K"
    "4096x2160  (256:135)  4K - DCI Full Frame"))

(defconst lib/ffmpeg-codecs
  '(("x265" . "libx265 -x265-params lossless=1 -preset medium")
    ("ProRes" . "prores -profile:v 3 -preset medium")))

;;;###autoload
(defun lib/ffmpeg-make-timelapse (sequence-dir framerate out-resolution codec out &optional overwrite)
  (interactive
   (list (read-directory-name "Sequence directory (*.JPG): " default-directory nil 'must-match)
         (read-number "Framerate: " 24)
         (completing-read "Output resolution: " lib/ffmpeg-video-resolutions)
         (completing-read "Codec: " lib/ffmpeg-codecs)
         (read-file-name "Timelapse output file: " default-directory)))
  (let* ((sequence-dir (file-name-as-directory (file-truename sequence-dir)))
         (out (file-truename out))
         (out-resolution (car (split-string out-resolution " " 'omit-nulls)))
         (codec (cdr (assoc codec lib/ffmpeg-codecs #'equal)))
         (buffer (get-buffer-create "*ffmpeg*"))
         ;; I couldn't get ffmpeg to understand a case insensitive glob pattern,
         ;; thus the code should find the first file with a known extension and
         ;; get its actual extension.
         (glob-pattern (concat "*." (file-name-extension
                                     (car (directory-files sequence-dir nil
                                                           (rx (or ".jpg" ".JPG") line-end))))))
         (cmd (string-join (list "ffmpeg"
                                 (when overwrite "-y")
                                 (format "-framerate %s" framerate)
                                 "-pattern_type glob"
                                 (format "-i '%s'" (concat sequence-dir glob-pattern))
                                 (format "-s:v %s" out-resolution)
                                 (format "-c:v %s" codec)
                                 (format "'%s'" out))
                           " ")))
    (with-current-buffer buffer
      (goto-char (point-max))
      (insert "\n[INFO] " cmd "\n"))
    (start-process-shell-command "ffmpeg" buffer cmd)
    (display-buffer buffer)))

;;;###autoload
(defun lib/crunchyroll-download (url out-filename)
  "Download Crunchyroll episode URL to OUT-FILENAME."
  (interactive "sURL: \nFOutput: ")
  (let* ((username "icaro.ldm@gmail.com")
         (password (lib-util/read-password :host "crunchyroll.com" :login username))
         (out-filename (expand-file-name out-filename))
         (cmd (string-join (list "streamlink"
                                 "--ffmpeg-audio-transcode" "aac"
                                 "--ffmpeg-video-transcode" "x265"
                                 "--default-stream" "best"
                                 "--locale" "pt_BR"
                                 "--force"
                                 (format "--output \"%s\"" out-filename)
                                 (format "--crunchyroll-username '%s'" username)
                                 (format "--crunchyroll-password '%s'" password)
                                 (format "--url '%s'" url))
                           " ")))
    (with-current-buffer "*crunchyroll-download*"
      (insert "\n" cmd)
      (start-process-shell-command "crunchyroll" (current-buffer) cmd))))

;;;###autoload
(defun lib/generate-proxies-from-darktable (dir)
  (interactive "DDirectory: ")
  (let* ((files (directory-files-recursively dir (rx (or ".jpg" ".JPG") line-end)))
         (buf (get-buffer-create "*darktable-proxies*"))
         (max-width "1920")
         (proxies-dir (file-name-as-directory (expand-file-name "~/Pictures/Proxies")))
         (darktable-format (concat "nice -n 15 "
                                   (string-join (list "darktable-cli"
                                                      "'%s'"
                                                      (concat proxies-dir "'$(FILE_FOLDER)/$(FILE_NAME).jpg'")
                                                      "--hq" "true"
                                                      "--width" max-width
                                                      "--core --library" (expand-file-name "~/.config/darktable/library.db")
                                                      "--conf" "write_sidecar_files=FALSE"
                                                      "--conf" "plugins/lighttable/thumbnail_hq_min_level=never"
                                                      "--conf" "plugins/imageio/format/jpeg/quality=60"
                                                      "--conf" "plugins/imageio/storage/disk/overwrite=1")
                                                " ")))
         (error-msg-format "\n[ERROR] Command returned non-zero status code: %s\n"))
    (lib-util/promise-each
     (seq-map (lambda (file)
                (lambda (_)
                  (thread-first (promise-resolve nil)
                                (promise-then (lambda (_)
                                                (let ((darktable-cmd (format darktable-format (expand-file-name file))))
                                                  (with-current-buffer buf
                                                    (goto-char (point-max))
                                                    (insert (format "[INFO] %s\n" darktable-cmd)))
                                                  (promise:make-shell-command darktable-cmd))))
                                (promise-catch (lambda (reason)
                                                 (with-current-buffer buf
                                                   (goto-char (point-max))
                                                   (insert (format error-msg-format reason))))))))
              files))))

;;; YouTube

(defcustom lib/youtube-cookie-path
  "~/.config/youtube-dl/cookies.txt"
  "Path to YouTube cookies in Netscape format."
  :type 'string)

(defcustom lib/youtube-default-video-directory
  (file-name-directory (file-truename "~/Videos/"))
  "Default directory to store videos."
  :type 'string)

(defcustom lib/youtube-default-video-name
  "%(title)s.%(ext)s"
  "Default video name."
  :type 'string)

;;;###autoload
(defun lib/youtube-download-audio (url output)
  (interactive "sURL: \nFOutput path (*.flac): ")
  (cl-assert (file-exists-p (file-truename lib/youtube-cookie-path))
             nil "YouTube cookies file not found")
  (thread-first (lib-system/promise-start-process-shell-command
                 (string-join (list "yt-dlp"
                                    "--ignore-config"
                                    "--no-continue"
                                    "--no-mark-watched"
                                    "--no-overwrites"
                                    "--prefer-free-formats"
                                    "--print-traffic"
                                    "--restrict-filenames"
                                    "--extract-audio"
                                    "--audio-format" "flac"
                                    "--audio-quality" "0" ; Best (default is 5)
                                    "--limit-rate" "3.75M"
                                    (format "--cookies '%s'" (file-truename lib/youtube-cookie-path))
                                    (format "-o '%s'" output)
                                    (format "'%s'" url))
                              " "))
                (promise-then (lambda (_)
                                (message "YouTube audio extracted successfully.")))
                (promise-catch (lambda (err)
                                 (message "Failed to extract YouTube audio: '%s'" err)))))

(defun lib/youtube-url-from-clipboard ()
  "Returns URL from system clipboard if it's a YouTube host,
otherwise nil."
  (let* ((maybe-url (substring-no-properties (gui-get-selection)))
         (url (url-generic-parse-url maybe-url)))
    (when (string-match-p (rx "youtube.com" line-end)
                          (or (url-host url) ""))
      maybe-url)))

;;;###autoload
(defun lib/youtube-download-video (url &optional output)
  (interactive (list (read-string "URL: " (lib/youtube-url-from-clipboard)
                                  nil nil nil)
                     (read-file-name "Output: "
                                     lib/youtube-default-video-directory
                                     lib/youtube-default-video-name
                                     nil nil nil)))
  (let ((output (cond ((or (not output) (string-blank-p output))
                       (concat lib/youtube-default-video-directory
                               lib/youtube-default-video-name))
                      ((file-name-directory output)
                       (file-truename output))
                      (t (concat lib/youtube-default-video-directory
                                 output)))))
    (cl-assert (thread-first output file-name-directory file-directory-p)
               nil "Error: directory '%s' does not exist" (file-name-directory output))
    (cl-assert (file-exists-p (file-truename lib/youtube-cookie-path))
               nil "YouTube cookies file not found")
    (thread-first (lib-system/promise-start-process-shell-command
                   (string-join (list "yt-dlp"
                                      "--ignore-config"
                                      "--no-continue"
                                      "--no-mark-watched"
                                      "--no-overwrites"
                                      "--prefer-free-formats"
                                      "--print-traffic"
                                      "--restrict-filenames"
                                      "--remux-video" "mp4"
                                      "--merge-output-format" "mp4"
                                      "-f" "bestvideo+bestaudio" ; See https://askubuntu.colib/questions/486297/how-to-select-video-quality-from-youtube-dl
                                      (format "--cookies '%s'" (file-truename lib/youtube-cookie-path))
                                      (format "-o '%s'" output)
                                      (format "'%s'" url))
                                " "))
                  (promise-then (lambda (_)
                                  (message "YouTube video downloaded successfully.")))
                  (promise-catch (lambda (err)
                                   (message "Failed to download YouTube video: '%s'" err))))))

;;; Text-to-speech

(defvar lib/google-tts-supported-languages nil
  "Cached alist of all supported IETF language tags reported by
gtts-cli.")

(defvar lib/google-tts-history nil
  "Command history to choose IETF language tags.")

(defun lib-media/google-tts-get-supported-languages ()
  (or lib/google-tts-supported-languages
      (let* ((output (lib-util/call-process "gtts-cli --all"))
             (languages (thread-last (split-string (cdr output) "\n")
                                     (seq-map (lambda (s) (reverse (split-string s ":"))))
                                     (seq-map (lambda (e) (seq-map #'string-trim e)))
                                     (seq-sort-by #'car #'string-lessp))))
        (setq lib/google-tts-supported-languages languages)
        languages)))

(defun lib/google-tts-read-string (s language-tag speed)
  (cl-assert (not (string-blank-p s)) nil "Empty input")
  (let ((cmd (string-join (list "gtts-cli"
                                (format "--lang %s" language-tag)
                                (when speed "--slow")
                                (shell-quote-argument s)
                                "|"
                                (format "mpv --speed=%s --no-video --really-quiet -"
                                        (if (or (not speed) (equal speed 'slower))
                                            1.0
                                          speed)))
                          " ")))
    (let ((inhibit-message t))
      (message "Google TTS command: '%s'" cmd)
      (start-process-shell-command "google-tts" nil cmd))))

(defun lib/google-tts-preprocess (region)
  (with-temp-buffer
    (insert region)
    (lib-util/unfill-dwim (point-min) (point-max))
    (buffer-string)))

(defun lib/google-tts-choose-language ()
  (interactive)
  (let* ((languages (lib/google-tts-get-supported-languages))
         (key (completing-read "Language: " languages nil t nil 'lib/google-tts-history))
         (value (car (map-elt languages key))))
    (list key value)))

;;;###autoload
(defun lib/google-tts-read-region (&optional speed delay)
  "Read active region using Google Translate text-to-speech API.
See `lib-media/google-tts-dictate-region' for more details about
SPEED and DELAY."
  (interactive)
  (cl-assert (use-region-p) nil "No region selected")
  (pcase-let ((`(_ ,language-tag) (lib/google-tts-choose-language))
              (delay (or delay 0))
              (region  (buffer-substring-no-properties (region-beginning) (region-end))))
    (run-with-timer delay nil (lambda ()
                                (message "after %s" (buffer-substring-no-properties (region-beginning) (region-end)))
                                (thread-first
                                  (lib/google-tts-preprocess region)
                                  (lib/google-tts-read-string language-tag speed))))))

;;;###autoload
(defun lib/google-tts-dictate-region (speed delay)
  "Dictate active region using Google Translate text-to-speech API.

SPEED is the slowdown percentage and should be a number between 1
and 100. 100 is the normal (slower) speed generated by the
gtts-cli with the --slow flag. If SPEED is empty, then simply
generate audio using the gtts-cli --slow option.

DELAY is the number of seconds to wait before dictation starts.
The default is zero, i.e. read immediately."
  (interactive (list (read-string "Speed (%): ")
                     (read-number "Delay (seconds): " 0)))
  (let* ((speed (if (string-blank-p speed)
                    'slower
                  (let ((speed (string-to-number speed)))
                    (cl-assert (<= 1 speed 100) nil "Speed should be a number between 1 and 100")
                    (/ speed 100.0)))))
    (cl-assert (<= 0 delay 300) nil "Delay should be a number between 0 and 300 (inclusive)")
    (lib/google-tts-read-region speed delay)))

(provide 'lib-media)

;; Local Variables:
;; read-symbol-shorthands: (("lib/" . "lib-media/"))
;; End:
