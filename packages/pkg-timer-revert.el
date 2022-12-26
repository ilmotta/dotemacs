;;; -*- lexical-binding: t; -*-

(defun pkg-timer-revert/process-menu-setup ()
  (setq-local timer-revert-delay 5)
  (timer-revert-mode +1))

(my/package
  (timer-revert :ref "615c91dec8b440d2b9b7c725dd733d7432564e45")
  :defer t
  :hook (process-menu-mode-hook . pkg-timer-revert/process-menu-setup))

(provide 'pkg-timer-revert)
