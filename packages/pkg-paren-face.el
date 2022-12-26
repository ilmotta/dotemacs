;;; -*- lexical-binding: t; -*-

;;; Commentary:
;; Defines a face named `parenthesis' used just for parentheses. The intended
;; purpose of this face is to make parentheses less visible by dimming them.

;;; Code:

(my/package
  (paren-face :ref "bf741a6038a2554abf98d31e658421c33f8bf7a4")
  :defer t
  :hook ((emacs-lisp-mode-hook clojure-mode-hook) . paren-face-mode)
  :init
  (setq paren-face-regexp "[])}]"))

(provide 'pkg-paren-face)
