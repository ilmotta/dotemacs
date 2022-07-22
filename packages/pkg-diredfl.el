;;; -*- lexical-binding: t; -*-

;;; Commentary:
;; Extra font lock rules for a more colorful dired.

;;; Code:

(my/package diredfl
  :straight t
  :defer t
  :hook (dired-mode-hook . diredfl-mode))

(provide 'pkg-diredfl)
