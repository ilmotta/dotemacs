;;; -*- lexical-binding: t; -*-

;;; Commentary:
;; Display ugly form feed characters as tidy horizontal rules.

;;; Code:

(my/package page-break-lines
  :straight t
  :defer t
  :hook ((compilation-mode-hook
          emacs-lisp-mode-hook
          clojure-mode-hook
          help-mode-hook
          lisp-mode-hook
          outline-mode-hook
          scheme-mode-hook) . page-break-lines-mode))

(provide 'pkg-page-break-lines)
