;;; -*- lexical-binding: t; -*-

;;; Commentary:
;; Open files with external programs and integrated with dired actions. Use the
;; `openwith-make-extension-regexp' function to build regex strings.

;;; Code:

(my/package openwith
  :straight t
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
                    "setsid -w xdg-open"
                    '(file)))))

(provide 'pkg-openwith)
