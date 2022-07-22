;;; -*- lexical-binding: t; -*-

(my/package typescript-mode
  :straight t
  :defer t
  :init
  (setq typescript-indent-level 2)
  (setq typescript-expr-indent-offset 0))

(provide 'pkg-typescript-mode)
