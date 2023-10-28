;;; -*- lexical-binding: t; -*-

;;; Commentary:
;; Show elisp evaluation results in an overlay.

;;; Code:

(require 'lib-util)

(defun pkg-xterm-color/eshell-h ()
  ;; This is necessary to print colors for certain programs (e.g. Jest).
  (setenv "TERM" "xterm-256color")

  ;; Let xterm-color handle colors.
  (setq eshell-output-filter-functions
        (remove 'eshell-handle-ansi-color eshell-output-filter-functions))

  ;; `xterm-color-filter' supports more colors than `ansi-color-apply'.
  (setq eshell-preoutput-filter-functions '(xterm-color-filter))
  (setq xterm-color-preserve-properties t))

(lib-util/pkg xterm-color
  :elpaca (:ref "1a4012854c69a5cdaeb5a73d2ad705011892fca3")
  :defer t
  :hook (eshell-before-prompt-hook . pkg-xterm-color/eshell-h))

(provide 'pkg-xterm-color)
