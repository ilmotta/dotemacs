;;; -*- lexical-binding: t; -*-

(require 'lib-util)

(lib-util/pkg web-mode
  :ensure (:ref "53bed1e6a8554da877c27ffad6bd65113dc758e3")
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
