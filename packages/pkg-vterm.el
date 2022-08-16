;;; -*- lexical-binding: t; -*-

(my/package vterm
  :if (executable-find "vterm")
  :straight t
  :defer t
  :commands (vterm vterm-other-window)

  :hook (vterm-mode-hook . lib-system/set-no-process-query-on-exit)

  :init
  (setq vterm-max-scrollback 9999))

(provide 'pkg-vterm)
