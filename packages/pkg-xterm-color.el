;;; -*- lexical-binding: t; -*-

;;; Commentary:
;; Show elisp evaluation results in an overlay.

;;; Code:

(defun pkg-xterm-color/eshell-h ()
  ;; This is necessary to print colors for certain programs (e.g. Jest).
  (setenv "TERM" "xterm-256color")

  ;; Let xterm-color handle colors.
  (setq eshell-output-filter-functions
        (remove 'eshell-handle-ansi-color eshell-output-filter-functions))

  ;; `xterm-color-filter' supports more colors than `ansi-color-apply'.
  (setq eshell-preoutput-filter-functions '(xterm-color-filter))
  (setq xterm-color-preserve-properties t))

(my/package xterm-color
  :straight t
  :defer t
  :hook (eshell-before-prompt-hook . pkg-xterm-color/eshell-h))

(provide 'pkg-xterm-color)
