;;; -*- lexical-binding: t; -*-

;;; Commentary:
;; Emacs client/library for Debug Adapter Protocol is a wire protocol for
;; communication between client and Debug Server.

;;; Code:

(my/package
  (dap-mode :ref "39bfaf1a3400b3ca4e9755f4d15e33abb0dda2c4")
  :defer t

  :init
  (general-def
    :keymaps 'dap-mode-map
    :states 'normal
    :prefix (concat my/local-leader "d")
    "."  #'dap-breakpoint-toggle
    "c"  #'dap-continue
    "i"  #'dap-step-in
    "m"  #'dap-hydra
    "n"  #'dap-next
    "o"  #'dap-step-out
    "s"  #'dap-debug
    "ub" #'dap-ui-breakpoints-list
    "ue" #'dap-ui-expressions
    "ul" #'dap-ui-locals
    "ur" #'dap-ui-repl
    "us" #'dap-ui-sessions
    "x"  #'dap-disconnect)

  (setq dap-auto-configure-features
        '(;; sessions    ; Show sessions popup window when debugging
          ;; locals      ; Show locals popup window when debugging
          ;; breakpoints ; Show breakpoints popup window when debugging
          ;; expressions ; Show expressions popup window when debugging
          repl           ; Show REPL popup window when debugging
          ;; tooltip     ; Enable `dap-tooltip-mode` that enables mouse hover support when debugging
          ;; controls    ; Enable `dap-ui-controls-mode` with controls to manage the debug session when debugging
          ))

  (setq dap-breakpoints-file (concat my/cache-dir "dap-breakpoints"))

  ;; Note: `dap-mode' will be enabled automatically when lsp-mode is enabled.
  :config
  (require 'dap-dlv-go))

(provide 'pkg-dap-mode)
