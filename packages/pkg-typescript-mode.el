;;; -*- lexical-binding: t; -*-

(my/package
  typescript-mode
  :defer t
  :hook (typescript-mode-hook . electric-pair-local-mode)
  :init
  (setq typescript-indent-level 2)
  (setq typescript-expr-indent-offset 0))

(provide 'pkg-typescript-mode)
