;;; -*- lexical-binding: t; -*-
(require 'use-package)

;; Minor mode for `scheme-mode' buffers that provides highlighting and
;; indentation rules for Guix Guile code, as well as some tools to work with
;; Guix (or even an arbitrary Guile code) with Geiser.
(unless my/windows?
  (my/package guix-devel
    :no-require t
    :defer t
    :hook (scheme-mode-hook . guix-devel-mode)))

(provide 'pkg-guix-devel)
