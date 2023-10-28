;;; -*- lexical-binding: t; -*-
(require 'use-package)

;; Emacs start-up profiler.
(lib-util/pkg esup
  :straight t
  :defer t
  :init
  (setq esup-child-max-depth 1)
  (setq esup-depth 1)
  (setq esup-insignificant-time 0.009))

(provide 'pkg-esup)
