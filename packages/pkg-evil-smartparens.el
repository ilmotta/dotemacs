;;; -*- lexical-binding: t; -*-

(lib-util/pkg evil-smartparens
  :elpaca (:ref "026d4a3cfce415a4dfae1457f871b385386e61d3")
  :defer t
  :init
  (when (bound-and-true-p evil-mode)
    (seq-doseq (hook my/lisp-modes-hooks)
      (add-hook hook #'evil-smartparens-mode))))

(provide 'pkg-evil-smartparens)
