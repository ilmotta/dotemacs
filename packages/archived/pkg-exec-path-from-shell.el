;;; -*- lexical-binding: t; -*-
(require 'use-package)

;; This package helps me configure Emacs GUI to use the same variables set by
;; zshell. For example, this is important to use FZF (fuzzy finder).
(lib-util/pkg exec-path-from-shell
  :when (memq window-system '(mac ns x))
  :straight t

  ;; Not needed with NixOS.
  :disabled t

  :demand t
  :config
  ;; Do not warn if variables are being set in the wrong shell startup files.
  (setq exec-path-from-shell-check-startup-files nil)

  ;; Don't start an interactive shell.
  (setq exec-path-from-shell-arguments nil)

  ;; For security reasons, copy only the absolute minimum environment
  ;; variables to Emacs GUI.
  (setq exec-path-from-shell-variables
        '("PATH"

          ;; This variable should be set to "GNOME" before running
          ;; gnome-control-center.
          "XDG_CURRENT_DESKTOP"))
  (exec-path-from-shell-initialize))

(provide 'pkg-exec-path-from-shell)
