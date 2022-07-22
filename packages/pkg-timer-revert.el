;;; -*- lexical-binding: t; -*-

(defun pkg-timer-revert/process-menu-setup ()
  (setq-local timer-revert-delay 5)
  (timer-revert-mode +1))

(my/package timer-revert
  :straight t
  :defer t
  :hook (process-menu-mode-hook . pkg-timer-revert/process-menu-setup))

(provide 'pkg-timer-revert)
