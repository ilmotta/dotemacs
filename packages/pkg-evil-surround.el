;;; -*- lexical-binding: t; -*-

;; Surround.vim for evil.
(my/package evil-surround
  :straight t
  :defer t

  :init
  (with-eval-after-load 'evil
    (global-evil-surround-mode))

  :config
  (add-to-list 'evil-surround-operator-alist '(evil-cp-change . change))
  (add-to-list 'evil-surround-operator-alist '(evil-cp-delete . delete)))

(provide 'pkg-evil-surround)
