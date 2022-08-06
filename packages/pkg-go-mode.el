;;; -*- lexical-binding: t; -*-

(my/package go-mode
  :straight t
  :defer t
  :hook (go-mode-hook . electric-pair-local-mode)
  :init
  (my/general-mode-def
    :keymaps '(go-mode-map)
    "g a" '(go-goto-arguments :properties (:jump t))
    "g d" '(go-goto-docstring :properties (:jump t))
    "g f" '(go-goto-function-name :properties (:jump t))
    "g i" '(go-goto-imports :properties (:jump t))
    "h ." '(godoc-at-point :properties (:jump t))
    "i a" #'go-import-add
    "i c" #'go-remove-unused-imports))

(provide 'pkg-go-mode)
