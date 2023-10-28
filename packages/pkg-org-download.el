;;; -*- lexical-binding: t; -*-

;;; Commentary:
;; Easily insert images in Org buffers (e.g. drag & drop, kill-ring).

;;; Code:

(defun pkg-org-download/file-format-default (filename)
  "Format the file name. If it's a screenshot, then onl use the
`org-download-timestamp' as the basename."
  (if (equal "screenshot" (file-name-base filename))
      (format "%s.%s"
              (format-time-string org-download-timestamp)
              (file-name-extension filename))
    (concat (format-time-string org-download-timestamp)
            filename)))

(lib-util/pkg org-download
  :elpaca (:ref "19e166f0a8c539b4144cfbc614309d47a9b2a9b7")
  :defer t
  :commands (org-download-screenshot org-download-yank)
  :hook ((dired-mode-hook org-mode-hook) . org-download-enable)
  :init
  ;; Store images in a separate folder (relative to file).
  (setq org-download-image-dir "./images")

  (setq org-download-abbreviate-filename-function #'file-relative-name)
  (setq org-download-file-format-function #'pkg-org-download/file-format-default)
  (setq org-download-link-format "[[file:%s]]\n")
  (setq org-download-screenshot-file "/tmp/screenshot.png")
  (setq org-download-timestamp my/note-id-time-string)

  ;; Don't add image width attributes because we'll add them uppercased.
  (setq org-download-image-org-width 0
        org-download-image-html-width 0
        org-download-image-attr-list '("#+ATTR_ORG: :width 800"))

  ;; When non-nil display inline images in org buffer after download.
  (setq org-download-display-inline-images t)

  ;; Control the second part of the file name. When nil don't use the heading
  ;; level to create a subdirectory.
  (setq org-download-heading-lvl nil))

(provide 'pkg-org-download)
