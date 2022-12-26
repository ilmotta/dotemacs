;;; -*- lexical-binding: t; -*-

;;; Commentary:
;; Emacs client/library for Debug Adapter Protocol is a wire protocol for
;; communication between client and Debug Server. It’s similar to the LSP but
;; provides integration with debug server.

;;; Code:

(my/package
  (dap-mode :ref "512b70bb71b5727bfb155f08f7e9a32f0496f1a6")
  :defer t
  :init
  (setq dap-breakpoints-file (concat my/cache-dir "dap-breakpoints")))

(provide 'pkg-dap-mode)
