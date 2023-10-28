;;; -*- lexical-binding: t; -*-
(require 'use-package)

;; This package implements support for mapping a pair of simultaneously pressed
;; keys to a command and for mapping the same key being pressed twice in quick
;; succession to a command.
(lib-util/pkg key-chord
  :straight t
  :demand t
  :disabled t ; Not in use anymore.
  :config
  (setq key-chord-one-key-delay 0.18)
  (setq key-chord-two-keys-delay 0.1)

  ;; (with-eval-after-load 'evil
  ;;   (key-chord-define evil-normal-state-map ",," #'switch-to-buffer))

  (key-chord-mode +1))

(provide 'pkg-key-chord)
