;;; -*- lexical-binding: t; -*-
(require 'use-package)

(my/package magit-delta
  :straight t
  ;; Too slow when rendering diffs in the magit status buffer.
  :disabled t
  :hook (magit-mode-hook . magit-delta-mode)
  :defer t
  :config
  ;; Use "delta --show-syntax-themes" to see all available syntax themes.
  (setq magit-delta-default-dark-theme "zenburn")
  (setq magit-delta-default-light-theme "GitHub")
  (setq magit-delta-hide-plus-minus-markers nil))

(provide 'pkg-magit-delta)
