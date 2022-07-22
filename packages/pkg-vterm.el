;;; -*- lexical-binding: t; -*-

(my/package vterm
  :if (executable-find "vterm")
  :straight t
  :defer t
  :commands (vterm vterm-other-window)
  :init
  (setq vterm-max-scrollback 9999))

(provide 'pkg-vterm)
