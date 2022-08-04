;;; -*- lexical-binding: t; -*-

;;; Commentary:
;; Allows Emacs to copy to and paste from the GUI clipboard when running in text
;; terminal.

;;; Code:

(my/package xclip
  :if (and (not (display-graphic-p)) (not my/android?))
  :straight t
  :demand t
  :config
  (xclip-mode +1))

(provide 'pkg-xclip)
