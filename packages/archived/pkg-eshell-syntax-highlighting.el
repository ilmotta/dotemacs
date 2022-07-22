;;; -*- lexical-binding: t; -*-
(require 'use-package)

(my/package eshell-syntax-highlighting
  :straight t
  ;; Unfortunately it doesn't work with evil. There's no issue on Github and I
  ;; don't have the time to fix it.
  :disabled t
  :hook (eshell-mode-hook . eshell-syntax-highlighting-mode))

(provide 'pkg-eshell-syntax-highlighting)
