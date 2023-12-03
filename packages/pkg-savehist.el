;;; -*- lexical-binding: t; -*-

;;; Commentary:
;; Persistent minibuffer history.

(require 'lib-util)

(lib-util/pkg savehist
  :elpaca nil
  :init
  (add-hook 'elpaca-after-init-hook #'savehist-mode)

  (setq savehist-file (file-name-concat my/cache-dir "savehist"))
  (setq savehist-save-minibuffer-history t)

  ;; Save on kill only.
  (setq savehist-autosave-interval nil)

  ;; We don't need to add minibuffer history variables to this list.
  (setq savehist-additional-variables '(search-ring regexp-search-ring)))

(provide 'pkg-savehist)
