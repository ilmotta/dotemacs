;;; -*- lexical-binding: t; -*-

;;; Commentary:
;; Shows the recursive size of directories in Dired buffers. If the =du= program
;; is available, then directory sizes are obtained with it, otherwise it falls
;; back to ELisp.

;;; Code:

(require 'lib-util)

(lib-util/pkg dired-du
  :elpaca (:host github
           :repo "emacsmirror/dired-du"
           :ref "f7e1593e94388b0dfb71af8e9a3d5d07edf5a159"
           :branch nil)
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
