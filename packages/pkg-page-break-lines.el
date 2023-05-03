;;; -*- lexical-binding: t; -*-

;;; Commentary:
;; Display ugly form feed characters as tidy horizontal rules.

;;; Code:

(my/package
  (page-break-lines :ref "79eca86e0634ac68af862e15c8a236c37f446dcd")
  :defer t
  :hook ((c-mode-hook
          compilation-mode-hook
          emacs-lisp-mode-hook
          clojure-mode-hook
          help-mode-hook
          lisp-mode-hook
          outline-mode-hook
          scheme-mode-hook) . page-break-lines-mode))

(provide 'pkg-page-break-lines)
