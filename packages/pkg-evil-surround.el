;;; -*- lexical-binding: t; -*-

;; Surround.vim for evil.
(my/package
  (evil-surround :ref "c9e1449bf3f740b5e9b99e7820df4eca7fc7cf02")
  :defer t

  :init
  (with-eval-after-load 'evil
    (global-evil-surround-mode))

  :config
  (add-to-list 'evil-surround-operator-alist '(evil-cp-change . change))
  (add-to-list 'evil-surround-operator-alist '(evil-cp-delete . delete)))

(provide 'pkg-evil-surround)
