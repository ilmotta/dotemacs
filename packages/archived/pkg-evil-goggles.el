;;; -*- lexical-binding: t; -*-
(require 'use-package)

(lib-util/pkg evil-goggles
  :straight t

  ;; This package is incompatible with evil-cleverparens and evil-smartparens.
  ;; For some reason, certain basic operations don't work in other modes using
  ;; pure evil.
  ;;
  ;; https://github.com/edkolev/evil-goggles/issues/25
  :disabled t

  :config
  (setq evil-goggles-pulse #'display-graphic-p)
  (setq evil-goggles-duration 0.1)

  ;; This variable affects "blocking" hints, for example when deleting - the
  ;; hint is displayed, the deletion is delayed (blocked) until the hint
  ;; disappers, then the hint is removed and the deletion executed; it makes
  ;; sense to have this duration short.
  (setq evil-goggles-blocking-duration nil)

  ;; This variable affects "async" hints, for example when indenting - the
  ;; indentation is performed with the hint visible, i.e. the hint is displayed,
  ;; the action (indent) is executed (asynchronous), then the hint is removed,
  ;; highlighting the result of the indentation.
  (setq evil-goggles-async-duration nil)

  (setq evil-goggles-enable-change t)
  (setq evil-goggles-enable-commentary t)
  (setq evil-goggles-enable-delete t)
  (setq evil-goggles-enable-fill-and-move t)
  (setq evil-goggles-enable-indent t)
  (setq evil-goggles-enable-join t)
  (setq evil-goggles-enable-nerd-commenter t)
  (setq evil-goggles-enable-paste t)
  (setq evil-goggles-enable-record-macro t)
  (setq evil-goggles-enable-replace-with-register t)
  (setq evil-goggles-enable-set-marker t)
  (setq evil-goggles-enable-shift t)
  (setq evil-goggles-enable-surround t)
  (setq evil-goggles-enable-yank t)

  (evil-goggles-use-diff-faces)
  (evil-goggles-mode))

(provide 'pkg-evil-goggles)
