;;; -*- lexical-binding: t; -*-

(defun pkg-ledger-mode/report-balance ()
  (interactive)
  (ledger-report "bal" nil))

(my/package ledger-mode
  :elpaca (:ref "8bad528d43007e0310b5e72e6e021b502b30495c")
  :defer t
  :mode ((rx ".ledger" string-end) . ledger-mode)
  :init
  (general-def
    :keymaps 'ledger-mode-map
    "M-j" #'ledger-navigate-next-xact-or-directive
    "M-k" #'ledger-navigate-prev-xact-or-directive
    "M-J" #'ledger-navigate-next-uncleared
    "M-K" #'ledger-navigate-previous-uncleared)

  (general-def
    :keymaps 'ledger-mode-map
    :states 'normal
    :prefix my/local-leader
    "r b" #'pkg-ledger-mode/report-balance)

  (setq ledger-mode-should-check-version nil)

  (setq ledger-reports
        '(("bal" "%(binary) -f %(ledger-file) bal --real --effective --verify --payee-width 40 --wide  Liabilities Assets")
          ("reg" "%(binary) -f %(ledger-file) reg")
          ("payee" "%(binary) -f %(ledger-file) reg @%(payee)")
          ("account" "%(binary) -f %(ledger-file) reg %(account)"))))

(provide 'pkg-ledger-mode)
