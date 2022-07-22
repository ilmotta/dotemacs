;;; -*- lexical-binding: t; -*-
(require 'use-package)

(my/package restart-emacs
  ;; Disabled because there's no way in Emacs to restart and reuse the same
  ;; command line arguments of the previous instance. In NixOS this means the
  ;; new instance cannot find certain packages, like vterm or pdf-tools.
  ;;
  ;; See https://github.com/iqbalansari/restart-emacs/issues/11, which is even
  ;; documented in the README.
  :disabled t
  :straight t
  :defer t
  :commands (restart-emacs)
  :init
  (general-def "C-x C-r" #'restart-emacs))

(provide 'pkg-restart-emacs)
