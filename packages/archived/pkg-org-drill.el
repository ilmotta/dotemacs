;;; -*- lexical-binding: t; -*-
(require 'use-package)

;; Flashcards and spaced repetition for org-mode.
(my/package org-drill
  :when (version< "28.0.50" emacs-version) ; Native compilation fails
  :straight t
  :defer t
  :init
  (setq org-drill-use-visible-cloze-face-p t)
  (setq org-drill-maximum-items-per-session nil))

(provide 'pkg-org-drill)
