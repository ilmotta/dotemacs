;;; -*- lexical-binding: t; -*-

(require 'lib-util)

(defun pkg-flycheck-ledger/enable-mode ()
  (require 'flycheck-ledger))

(lib-util/pkg flycheck-ledger
  :elpaca (:ref "628e25ba66604946085571652a94a54f4d1ad96f")
  :defer t
  :hook (ledger-mode-hook . flycheck-mode)
  :hook (ledger-mode-hook . pkg-flycheck-ledger/enable-mode))

(provide 'pkg-flycheck-ledger)
