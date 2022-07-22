;;; -*- lexical-binding: t; -*-

(my/package json-mode
  :straight t
  :defer t
  :init
  (my/general-mode-def
    :keymaps '(json-mode-map)
    "p p" #'json-mode-show-path
    "p y" #'json-mode-kill-path))

(provide 'pkg-json-mode)
