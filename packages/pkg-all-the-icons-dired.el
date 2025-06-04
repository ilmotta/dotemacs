;;; -*- lexical-binding: t; -*-

(require 'lib-util)

;; Adds dired support to `all-the-icons'.
(lib-util/pkg all-the-icons-dired
  :ensure (:ref "4564bec6bd3fd02dd870e6d2cfed37fe38bbc93a")
  :when (display-graphic-p)
  :defer t
  :hook (dired-mode-hook . all-the-icons-dired-mode))

(provide 'pkg-all-the-icons-dired)
