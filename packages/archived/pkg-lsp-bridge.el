;;; -*- lexical-binding: t; -*-

;;; Commentary:
;; Review on 2022-06-26. The completion list is extremely fast. It kind of
;; works, but I noticed far too many bugs to use it as a replacement.

;;; Code:
(require 'use-package)

(my/package epc
  :straight (:host github :repo "kiwanami/emacs-epc")
  :demand t)

(my/package lsp-bridge
  :straight (:host github
             :repo "manateelazycat/lsp-bridge"
             :files (:defaults "*.py" "langserver" "acm"))
  :demand t
  :init
  ;; This version of python should have epc installed (pip install epc).
  (setq lsp-bridge-python-command (concat my/cache-dir "venv/bin/python"))

  (setq lsp-bridge-epc-accept-process-timeout 150)

  (setq lsp-bridge-diagnostics-fetch-idle 1)
  (setq lsp-bridge-enable-candidate-doc-preview t)
  (setq lsp-bridge-enable-signature-help t)
  (setq acm-enable-doc t)
  (setq acm-enable-icon t)
  (setq acm-candidate-match-function 'orderless-regexp)

  ;; Debugging.
  (setq lsp-bridge-enable-log nil)
  (setq lsp-bridge-enable-debug nil)
  (setq lsp-bridge-epc-debug nil))

(provide 'pkg-lsp-bridge)
