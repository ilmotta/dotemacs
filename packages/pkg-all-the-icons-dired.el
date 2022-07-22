;;; -*- lexical-binding: t; -*-

;; Adds dired support to `all-the-icons'.
(my/package all-the-icons-dired
  :when (display-graphic-p)
  :straight t
  :defer t
  :hook (dired-mode-hook . all-the-icons-dired-mode)
  :config
  ;; Re-align icons after creating files/directories.
  ;; Issue: https://github.com/jtbm37/all-the-icons-dired/issues/34
  (advice-add 'dired-do-create-files :around #'all-the-icons-dired--refresh-advice)
  (advice-add 'dired-create-directory :around #'all-the-icons-dired--refresh-advice))

(provide 'pkg-all-the-icons-dired)
