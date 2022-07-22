;;; -*- lexical-binding: t; -*-

(my/package evil-smartparens
  :straight t
  :defer t
  :init
  (when (bound-and-true-p evil-mode)
    (seq-doseq (hook my/lisp-modes-hooks)
      (add-hook hook #'evil-smartparens-mode))))

(provide 'pkg-evil-smartparens)
