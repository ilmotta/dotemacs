;;; -*- lexical-binding: t; -*-

;; Adds dired support to `all-the-icons'.
(my/package
  (all-the-icons-dired :ref "4564bec6bd3fd02dd870e6d2cfed37fe38bbc93a")
  :when (display-graphic-p)
  :defer t
  :hook (dired-mode-hook . all-the-icons-dired-mode))

(provide 'pkg-all-the-icons-dired)
