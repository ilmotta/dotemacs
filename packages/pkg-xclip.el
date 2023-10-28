;;; -*- lexical-binding: t; -*-

;;; Commentary:
;; Allows Emacs to copy to and paste from the GUI clipboard when running in text
;; terminal.

;;; Code:

(require 'lib-util)

(lib-util/pkg xclip
  :elpaca (:ref "a1ac607f75a250dddf49866918bb493884451130")
  :if (and (not (display-graphic-p)) (not my/android?))
  :demand t
  :config
  (xclip-mode +1))

(provide 'pkg-xclip)
