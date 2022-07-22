;;; -*- lexical-binding: t; -*-

;;; Commentary:
;; Show event history and command history of some or all buffers.

;;; Code:

(my/package command-log-mode
  :straight t
  :defer t
  :commands (command-log-mode
             clm/open-command-log-buffer
             clm/toggle-command-log-buffer)
  :init
  (setq command-log-mode-is-global t
        command-log-mode-key-binding-open-log nil
        command-log-mode-open-log-turns-on-mode t
        command-log-mode-window-size 50))

(provide 'pkg-command-log-mode)
