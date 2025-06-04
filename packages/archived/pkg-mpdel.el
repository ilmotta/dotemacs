;;; -*- lexical-binding: t; -*-
(require 'use-package)

(lib-util/pkg mpdel
  :straight t
  :disabled t ; TODO: Improve start-up time
  :demand t

  :general
  ;; Enable prefix key for Evil mode. Changing `mpdel-prefix-key' doesn't work.
  (:states 'normal :prefix my/leader "z" '(:keymap mpdel-core-map))

  ;; Clean-up keybindings I don't use.
  (:keymaps 'mpdel-core-map
   "P"   nil ; #'mpdel-core-insert-current-playlist
   "n"   nil ; #'next-line
   "B"   nil ; #'mpdel-song-small-decrement
   "F"   nil ; #'mpdel-song-small-increment
   "b"   nil ; #'mpdel-song-normal-decrement
   "f"   nil ; #'mpdel-song-normal-increment
   "M-b" nil ; #'mpdel-song-large-decrement
   "M-f" nil ; #'mpdel-song-large-increment
   "+"   nil ; #'mpdel-core-volume-increase
   )

  ;; Replace keybindings conflicting with my Evil mode settings in modes derived
  ;; from `mpdel-tablist-mode'.
  (:keymaps 'mpdel-core-map
   "h"   #'mpdel-browser-open           ":" nil
   "i"   #'mpdel-song-open              "v" nil
   "p"   #'libmpdel-playback-play-pause "SPC" nil
   ">"   #'libmpdel-playback-next       "M-n" nil
   "<"   #'libmpdel-playback-previous   "M-p" nil
   "-"   #'navigel-open-parent          ; #'mpdel-core-volume-decrease
   "."   #'my/mpdel-cycle-playback-mode
   "g h" #'mpdel-core-open-artists
   "g a" #'mpdel-core-open-artists
   "g b" #'mpdel-core-open-albums
   "g d" #'mpdel-core-open-directories
   "g l" #'mpdel-core-open-stored-playlists)

  (:keymaps 'mpdel-tablist-mode-map
   :states 'normal
   ;; Keybindings shadowed by evil-mode.
   "TAB" #'tablist-forward-column
   "DEL" #'tablist-unmark-backward
   "<"   #'tablist-shrink-column
   ">"   #'tablist-enlarge-column
   "m"   #'tablist-mark-forward
   "j"   #'tablist-next-line
   "k"   #'tablist-previous-line
   "t"   #'tablist-toggle-marks
   "u"   #'tablist-unmark-forward

   ;; Commands available in all modes derived from `mpdel-tablist-mode'.
   "I"  #'mpdel-core-insert-current-playlist
   "-"  #'navigel-open-parent
   "i"  #'mpdel-song-open
   "a"  #'mpdel-core-add-to-current-playlist
   "A"  #'mpdel-core-add-to-stored-playlist
   "r"  #'mpdel-core-replace-current-playlist
   "R"  #'mpdel-core-replace-stored-playlist

   ;; Seek keybindings.
   "<kp-add>"        #'mpdel-song-normal-increment
   "M-<kp-add>"      #'mpdel-song-small-increment
   "S-<kp-add>"      #'mpdel-song-large-increment
   "<kp-subtract>"   #'mpdel-song-normal-decrement
   "M-<kp-subtract>" #'mpdel-song-small-decrement
   "S-<kp-subtract>" #'mpdel-song-large-decrement)

  (:keymaps 'mpdel-song-mode-map
   :states 'normal
   "A"   #'mpdel-core-add-to-stored-playlist
   "l"   #'mpdel-playlist-open
   "p"   #'libmpdel-playback-play-pause
   "q"   #'mpdel-song-quit-window
   ">"   #'libmpdel-playback-next     "M-n" nil
   "<"   #'libmpdel-playback-previous "M-p" nil
   "g h" #'mpdel-core-open-artists
   "g a" #'mpdel-core-open-artists
   "g b" #'mpdel-core-open-albums
   "g d" #'mpdel-core-open-directories
   "g l" #'mpdel-core-open-stored-playlists

   ;; Seek keybindings.
   "<kp-add>"        #'mpdel-song-normal-increment
   "M-<kp-add>"      #'mpdel-song-small-increment
   "S-<kp-add>"      #'mpdel-song-large-increment
   "<kp-subtract>"   #'mpdel-song-normal-decrement
   "M-<kp-subtract>" #'mpdel-song-small-decrement
   "S-<kp-subtract>" #'mpdel-song-large-decrement)

  (:keymaps 'mpdel-playlist-mode-map
   :states 'normal
   ;; Disable keybindings that don't make sense in the playlist mode.
   "C" nil
   "I" nil

   "M-j" #'mpdel-playlist-move-down
   "M-k" #'mpdel-playlist-move-up
   "RET" #'mpdel-playlist-play
   "o"   #'mpdel-playlist-play
   "p"   #'libmpdel-playback-play-pause
   ">"   #'libmpdel-playback-next     "M-n" nil
   "<"   #'libmpdel-playback-previous "M-p" nil
   "g h" #'mpdel-core-open-artists
   "g a" #'mpdel-core-open-artists
   "g b" #'mpdel-core-open-albums
   "g d" #'mpdel-core-open-directories
   "g l" #'mpdel-core-open-stored-playlists)

  ;; Use number pad to increment/decrement songs.
  (:keymaps 'override
   :states 'normal
   "s-<kp-add>"        #'mpdel-song-normal-increment
   "s-M-<kp-add>"      #'mpdel-song-small-increment
   "s-S-<kp-add>"      #'mpdel-song-large-increment
   "s-<kp-subtract>"   #'mpdel-song-normal-decrement
   "s-M-<kp-subtract>" #'mpdel-song-small-decrement
   "s-S-<kp-subtract>" #'mpdel-song-large-decrement)

  ;; Make sure the modeline is updated whenever the MPD state changes.
  :hook ((libmpdel-current-song-changed-hook
          libmpdel-player-changed-hook) . my/libmpdel-current-song-changed-hook)

  :preface
  (defun my/libmpdel-current-song-changed-hook ()
    "Force update the MPD modeline segment."
    (setq my/mpdel-modeline-force-update-p t))

  ;; Client for Music Player Daemon (MPD): a flexible, powerful, server-side
  ;; application for playing music.
  (defconst my/mpdel-regex-status-playback-modes
    (rx line-start
        "volume:" (one-or-more digit) "%"
        (one-or-more whitespace) "repeat:"  (one-or-more whitespace) (group (or "on" "off"))
        (one-or-more whitespace) "random:"  (one-or-more whitespace) (group (or "on" "off"))
        (one-or-more whitespace) "single:"  (one-or-more whitespace) (group (or "on" "off"))
        (one-or-more whitespace) "consume:" (one-or-more whitespace) (or "on" "off")
        line-end))

  (defvar my/mpdel-playback-modes
    (let ((modes '(track playlist random disabled)))
      (nconc modes modes)
      modes)
    "Circular list of playback modes.")

  (defvar my/mpdel-modeline-force-update-p nil
    "Force MPD modeline segment to be updated.")

  (defvar my/mpdel-modeline-last-updated 0
    "Timestamp (float) when the modeline segment was last updated.

Start it with zero to make sure the modeline is updated when
`my/mpdel-modeline-segment' is called the first time.")

  (defvar my/mpdel-modeline-segment nil
    "The last used modeline segment string.")

  (defconst my/mpdel-modeline-update-frequency 30
    "The minimum interval (in seconds) the modeline segment should be updated.

Unless `my/mpdel-modeline-force-update-p' is non-nil, the
MPD segment will use the previously stored string in
`my/mpdel-modeline-segment' and only update it after the
configured number of seconds has passed. If you have other
clients updating MPD you may want to lower it, but avoid lowering
too much, e.g. 1s.")

  (defun my/mpdel--enabled-playback-modes ()
    "Return a list of enabled playback modes."
    (let ((line (car (seq-reverse
                      (split-string
                       (string-trim-right (shell-command-to-string "mpc")) "\n")))))
      (when (string-match my/mpdel-regex-status-playback-modes line)
        (let ((modes '()))
          (when (equal "on" (match-string 1 line)) (push 'repeat modes))
          (when (equal "on" (match-string 2 line)) (push 'random modes))
          (when (equal "on" (match-string 3 line)) (push 'single modes))
          modes))))

  (defun my/mpdel--playback-mode ()
    "Return the current playback mode."
    (let ((mpd-modes (my/mpdel--enabled-playback-modes)))
      (cond ((equal mpd-modes '(random)) 'random)
            ((equal mpd-modes '(repeat)) 'playlist)
            ((equal (sort mpd-modes #'string<) '(repeat single)) 'track)
            (t 'disabled))))

  (defun my/mpdel-modeline-segment (&rest _args)
    "Return up-to-date MPD state every `my/mpdel-modeline-update-frequency'."
    (if (or my/mpdel-modeline-force-update-p
            (> (- (float-time) my/mpdel-modeline-last-updated)
               my/mpdel-modeline-update-frequency))
        (when-let* ((song (libmpdel-current-song)))
          (setq my/mpdel-modeline-force-update-p nil)
          (setq my/mpdel-modeline-last-updated (float-time))
          (let* ((title (s-truncate 20 (or (libmpdel-entity-name song) "") "\u2026") )
                 (artist (or (libmpdel-artist-name song) ""))
                 (artist-short (if (<= (length artist) 5)
                                   artist
                                 (string-join (seq-map (lambda (word)
                                                         (substring word 0 1))
                                                       (split-string artist " ")))))
                 (status (let ((play-state (libmpdel-play-state)))
                           (cond ((eq play-state 'pause) "\u23F8")
                                 ((eq play-state 'stop) "\u23F9")
                                 ((eq play-state 'play)
                                  (let ((mode (my/mpdel--playback-mode)))
                                    (cond ((eq mode 'track) "\u266A¹")
                                          ((eq mode 'playlist) "\u2B6F")
                                          ((eq mode 'random) "\x1f500")
                                          (t "\u266A"))))))))
            (setq my/mpdel-modeline-segment (format "%s %s" status title))))
      my/mpdel-modeline-segment))

;;;###autoload
  (defun my/mpdel-cycle-playback-mode ()
    "Cycle over MPD playback repeat modes.

Modes supported:
  - Random playback
  - Repeat song forever
  - Repeat playlist"
    (interactive)
    (let ((new-mode (car my/mpdel-playback-modes)))
      (cond ((eq new-mode 'disabled)
             (libmpdel-playback-set-single-never)
             (libmpdel-playback-unset-repeat)
             (libmpdel-playback-unset-random))
            ((eq new-mode 'track)
             (libmpdel-playback-set-repeat)
             (libmpdel-playback-set-single-forever)
             (libmpdel-playback-unset-random)
             (message "Playback repeat track enabled."))
            ((eq new-mode 'playlist)
             (libmpdel-playback-set-repeat)
             (libmpdel-playback-set-single-never)
             (libmpdel-playback-unset-random)
             (message "Playback repeat playlist enabled."))
            ((eq new-mode 'random)
             (libmpdel-playback-set-single-never)
             (libmpdel-playback-unset-repeat)
             (libmpdel-playback-set-random)
             (message "Playback random enabled.")))
      (setq my/mpdel-playback-modes (cdr my/mpdel-playback-modes))
      (setq my/mpdel-modeline-force-update-p t)))


  :config
  (with-eval-after-load 'doom-modeline
    (doom-modeline-def-segment my/mpdel
      (my/mpdel-modeline-segment))

    ;; All of these segments should be the same as the configuration in the
    ;; doom-modeline use-package declaration, except for the additional "my/mpdel"
    ;; segment.
    (doom-modeline-def-modeline 'main
      '(bar workspace-name window-number matches buffer-info remote-host buffer-position word-count parrot selection-info)
      '(my/mpdel objed-state misc-info persp-name battery grip irc mu4e gnus github debug lsp minor-modes input-method indent-info buffer-encoding major-mode process vcs checker))
    (doom-modeline-def-modeline 'special
      '(bar window-number matches buffer-info buffer-position word-count parrot selection-info)
      '(my/mpdel objed-state misc-info battery irc-buffers debug minor-modes input-method indent-info buffer-encoding major-mode process))
    (doom-modeline-def-modeline 'vcs
      '(bar window-number matches buffer-info buffer-position parrot selection-info)
      '(my/mpdel misc-info battery irc mu4e gnus github debug minor-modes buffer-encoding major-mode process))
    (doom-modeline-def-modeline 'message
      '(bar window-number matches buffer-info-simple buffer-position word-count parrot selection-info)
      '(my/mpdel objed-state misc-info battery debug minor-modes input-method indent-info buffer-encoding major-mode)))

  (setq libmpdel-music-directory "~/Music/Music/")

  (setq my/local-profile-name "LocalMPDServer"
        libmpdel-hostname "localhost"
        libmpdel-port 6600
        libmpdel-profiles `((,my/local-profile-name
                             ,libmpdel-hostname
                             ,libmpdel-port
                             ipv4)))

  (setq mpdel-core-volume-step 5)
  (setq mpdel-song-small-increment   "+1"
        mpdel-song-normal-increment  "+5"
        mpdel-song-large-increment  "+20"
        mpdel-song-small-decrement   "-1"
        mpdel-song-normal-decrement  "-5"
        mpdel-song-large-decrement  "-20")

  ;; Browser buffer UI.
  (setq mpdel-browser-top-level-entries
        '(directories
          empty-line
          artists
          albums
          empty-line
          stored-playlists
          current-playlist
          empty-line
          search-filter
          search-artist
          search-album
          search-title))

  (mpdel-mode +1)

  ;; Establish connection during Emacs start-up.
  (ignore-errors
    (libmpdel-connect-profile
     (libmpdel--get-profile-from-name my/local-profile-name))))

(provide 'pkg-mpdel)
