;;; -*- lexical-binding: t; -*-

;;; Commentary:
;; Shows the recursive size of directories in Dired buffers. If the =du= program
;; is available, then directory sizes are obtained with it, otherwise it falls
;; back to ELisp.

;;; Code:

(lib-util/pkg dired-du
  :elpaca (:ref "e5a2aa64849aae14fd6d1973919ec7e13ed76dd0")
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
