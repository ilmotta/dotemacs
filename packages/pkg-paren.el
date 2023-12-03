;;; -*- lexical-binding: t; -*-

(require 'lib-util)

(lib-util/pkg paren
  :elpaca nil
  :init
  (add-hook 'prog-mode-hook #'show-paren-mode)

  ;; By default, there’s a small delay before showing a matching parenthesis. It
  ;; can be deactivated with the following (which you have to do before activating
  ;; show-paren-mode).
  (setq show-paren-delay 0.125
        show-paren-highlight-openparen t
        show-paren-when-point-inside-paren t
        show-paren-when-point-in-periphery t

        ;; This new setting in Emacs 29.1 doesn't work well with Evil.
        show-paren-context-when-offscreen nil))

(provide 'pkg-paren)
