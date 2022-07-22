;;; -*- lexical-binding: t; -*-

;;; Commentary:
;; Show elisp evaluation results in an overlay.

;;; Code:

(my/package eros
  :straight t
  :defer t
  :hook ((clojure-mode-hook
          emacs-lisp-mode-hook
          scheme-mode-hook) . eros-mode))

(provide 'pkg-eros)
