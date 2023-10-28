;;; -*- lexical-binding: t; -*-

(require 'lib-util)

(lib-util/pkg typescript-mode
  :elpaca (:ref "4fcb4594819caf472ae42ea068a1c7795cf07f46")
  :hook (typescript-mode-hook . electric-pair-local-mode)
  :defer t
  :init
  (setq typescript-indent-level 2)
  (setq typescript-expr-indent-offset 0))

(provide 'pkg-typescript-mode)
