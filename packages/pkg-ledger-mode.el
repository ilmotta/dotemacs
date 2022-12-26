;;; -*- lexical-binding: t; -*-

(my/package
  (ledger-mode :ref "8bad528d43007e0310b5e72e6e021b502b30495c")
  :defer t
  :mode ((rx ".ledger" string-end) . ledger-mode)
  :init
  (general-def
    :keymaps 'ledger-mode-map
    "M-j" #'ledger-navigate-next-xact-or-directive
    "M-k" #'ledger-navigate-prev-xact-or-directive)

  (setq ledger-mode-should-check-version nil))

(provide 'pkg-ledger-mode)
