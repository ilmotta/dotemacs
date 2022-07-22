;;; -*- lexical-binding: t; -*-

(defun pkg-evil-org/setup-mode ()
  (when (bound-and-true-p evil-mode)
    (require 'evil-org)
    (require 'evil-org-agenda)
    (evil-org-set-key-theme)
    (evil-org-agenda-set-keys)
    (evil-org-mode +1)))

(my/package evil-org
  :straight t
  :defer t
  :hook (org-mode-hook . pkg-evil-org/setup-mode))

(provide 'pkg-evil-org)
