;;; -*- lexical-binding: t; -*-

;;; Commentary:
;; Easily open file, dired buffer or magit buffer in its respective remote host.

;;; Code:

(my/package
  (browse-at-remote :ref "010639fc6bd6c710b56e0f095352da60c92473a3")
  :defer t
  :init
  (general-def
    :keymaps 'pkg-magit/command-map
    "r" #'browse-at-remote))

(provide 'pkg-browse-at-remote)
