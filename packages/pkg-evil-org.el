;;; -*- lexical-binding: t; -*-

(defun pkg-evil-org/setup-mode ()
  (when (bound-and-true-p evil-mode)
    (require 'evil-org)
    (require 'evil-org-agenda)
    (evil-org-set-key-theme)
    (evil-org-agenda-set-keys)
    (evil-org-mode +1)))

(lib-util/pkg evil-org
  :elpaca (:ref "b1f309726b1326e1a103742524ec331789f2bf94")
  :defer t
  :disabled t ; Too many keybindings I don't really use.
  :hook (org-mode-hook . pkg-evil-org/setup-mode))

(provide 'pkg-evil-org)
