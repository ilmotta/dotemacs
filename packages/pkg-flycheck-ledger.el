;;; -*- lexical-binding: t; -*-

(defun pkg-flycheck-ledger/enable-mode ()
  (require 'flycheck-ledger))

(my/package flycheck-ledger
  :straight t
  :defer t
  :hook (ledger-mode-hook . flycheck-mode)
  :hook (ledger-mode-hook . pkg-flycheck-ledger/enable-mode))

(provide 'pkg-flycheck-ledger)
