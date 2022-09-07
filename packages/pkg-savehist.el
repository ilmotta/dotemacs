;;; -*- lexical-binding: t; -*-

(my/package savehist
  :straight (:type built-in)
  :defer t
  :hook (after-init-hook . savehist-mode)
  :init
  (setq savehist-file (concat my/cache-dir "savehist"))
  (setq savehist-save-minibuffer-history t)

  ;; Save on kill only.
  (setq savehist-autosave-interval nil)

  ;; We don't need to add minibuffer history variables to this list.
  (setq savehist-additional-variables '(search-ring regexp-search-ring)))

(provide 'pkg-savehist)
