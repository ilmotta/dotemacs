;;; -*- lexical-binding: t; -*-
(require 'use-package)

(my/package alchemist
  :straight t
  ;; I haven't used it in a long time. It's heavy to load and it hasn't received
  ;; updates in a long time (last commit on Mar/2018).
  :disabled t
  :defer t

  :config
  (with-eval-after-load 'company
    (pkg-company/set-backend 'alchemist-mode 'alchemist-company))

  ;; Remove global hooks set by alchemist. They run after
  ;; saving any buffer, which of course is not what I want.
  (remove-hook 'after-save-hook #'alchemist-hooks-test-on-save)
  (remove-hook 'after-save-hook #'alchemist-hooks-compile-on-save))

(provide 'pkg-alchemist)
