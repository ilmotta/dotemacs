;;; -*- lexical-binding: t; -*-

;;; Commentary:
;; Shows the recursive size of directories in Dired buffers. If the =du= program
;; is available, then directory sizes are obtained with it, otherwise it falls
;; back to ELisp.

;;; Code:

(my/package dired-du
  :straight t
  :defer t
  :commands (dired-du-mode)
  :init
  ;; Display file sizes using `file-size-human-readable'.
  (setq dired-du-size-format t)

  ;; Disable default bindings.
  (setq dired-du-bind-human-toggle nil
        dired-du-bind-count-sizes nil
        dired-du-bind-mode nil))

(provide 'pkg-dired-du)
