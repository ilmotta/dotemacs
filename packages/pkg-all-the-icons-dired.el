;;; -*- lexical-binding: t; -*-

;; Adds dired support to `all-the-icons'.
(my/package all-the-icons-dired
  :when (display-graphic-p)
  :straight t
  :defer t
  :hook (dired-mode-hook . all-the-icons-dired-mode))

(provide 'pkg-all-the-icons-dired)
