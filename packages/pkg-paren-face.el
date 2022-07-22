;;; -*- lexical-binding: t; -*-

;;; Commentary:
;; Defines a face named `parenthesis' used just for parentheses. The intended
;; purpose of this face is to make parentheses less visible by dimming them.

;;; Code:

(my/package paren-face
  :straight t
  :defer t
  :hook ((emacs-lisp-mode-hook clojure-mode-hook) . paren-face-mode)
  :init
  (setq paren-face-regexp "[])}]"))

(provide 'pkg-paren-face)
