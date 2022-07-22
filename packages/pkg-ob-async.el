;;; -*- lexical-binding: t; -*-

;;; Commentary:
;; Asynchronous source block execution for org-babel. `ob-async' depends on the
;; `emacs-async' package, which is a simple library for asynchronous processing
;; in Emacs.

;;; Code:

(defun pkg-ob-async/enable ()
  (require 'ob-async))

(my/package ob-async
  :straight t
  :defer t
  :hook (org-mode-hook . pkg-ob-async/enable))

(provide 'pkg-ob-async)
