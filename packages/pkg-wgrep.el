;;; -*- lexical-binding: t; -*-

;;; Commentary:
;; Allows you to edit a grep buffer and apply those changes to the file buffer
;; like sed interactively.

;;; Code:
(my/package wgrep
  :straight t
  :defer t
  :init
  (when my/evil-p
    (general-def
      :keymaps 'grep-mode-map
      :states 'normal
      "i" #'wgrep-change-to-wgrep-mode)))

(provide 'pkg-wgrep)
