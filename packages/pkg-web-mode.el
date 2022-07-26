;;; -*- lexical-binding: t; -*-

(my/package web-mode
  :straight t
  :defer t
  :mode (rx "." (or "html") string-end)

  :init
  (general-def
    :keymaps 'evil-normal-state-map
    [remap evil-toggle-fold] #'web-mode-fold-or-unfold)

  (setq web-mode-markup-indent-offset 2)

  ;; Auto-close on > and </
  (setq web-mode-auto-close-style 2)

  (setq web-mode-enable-auto-closing t)
  (setq web-mode-enable-auto-quoting nil))

(provide 'pkg-web-mode)
