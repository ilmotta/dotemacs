;;; -*- lexical-binding: t; -*-

;;; Commentary:
;; Easily open file, dired buffer or magit buffer in its respective remote host.

;;; Code:

(my/package browse-at-remote
  :straight t
  :defer t
  :init
  (general-def
    :keymaps 'pkg-magit/command-map
    "r" #'browse-at-remote))

(provide 'pkg-browse-at-remote)
