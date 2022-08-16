;;; -*- lexical-binding: t; -*-

(my/package vterm
  :unless my/android?
  :straight t
  :defer t

  :hook (vterm-mode-hook . lib-system/set-no-process-query-on-exit)

  :init
  (setq vterm-max-scrollback 9999))

(provide 'pkg-vterm)
