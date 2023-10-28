;;; -*- lexical-binding: t; -*-

;;; Commentary:
;; A simple command that takes a URL from the clipboard and inserts an org-mode
;; link with a title of a page found by the URL into the current buffer.

;;; Code:

(lib-util/pkg org-cliplink
  :elpaca (:ref "13e0940b65d22bec34e2de4bc8cba1412a7abfbc")
  :defer t
  :init
  (general-def
    :keymaps 'org-mode-map
    "C-c y" #'org-cliplink))

(provide 'pkg-org-cliplink)
