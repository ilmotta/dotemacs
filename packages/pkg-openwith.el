;;; -*- lexical-binding: t; -*-

;;; Commentary:
;; Open files with external programs and integrated with dired actions. Use the
;; `openwith-make-extension-regexp' function to build regex strings.

;;; Code:

(require 'lib-util)

(lib-util/pkg openwith
  :elpaca (:ref "1dc89670822966fab6e656f6519fdd7f01e8301a")
  :defer t
  :hook ((dired-mode-hook org-mode-hook) . openwith-mode)
  :init
  (setq openwith-associations
        (list (list (rx "." (or
                             ;; Audio/video
                             "avi"
                             "flv"
                             "m4a"
                             "mkv"
                             "mov"
                             "mp3"
                             "mp4"
                             "mpeg"
                             "mpg"
                             "ogg"
                             "ogm"
                             "opus"
                             "wav"
                             "webm"
                             "wmv"

                             ;; Documents
                             "epub"
                             "mobi"
                             "pdf")
                        line-end)
                    (if my/linux?
                        ;; Open program in a new session and wait until the
                        ;; program terminates.
                        "setsid -w xdg-open"
                      "open")
                    '(file)))))

(provide 'pkg-openwith)
