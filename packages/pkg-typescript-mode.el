;;; -*- lexical-binding: t; -*-

(lib-util/pkg typescript-mode
  :elpaca (:ref "4fcb4594819caf472ae42ea068a1c7795cf07f46")
  :defer t
  :hook (typescript-mode-hook . electric-pair-local-mode)
  :init
  (setq typescript-indent-level 2)
  (setq typescript-expr-indent-offset 0))

(provide 'pkg-typescript-mode)
