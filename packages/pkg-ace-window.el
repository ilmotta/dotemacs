;;; -*- lexical-binding: t; -*-

;;; Commentary:
;; A package for selecting a window to switch to. It aims to take the speed and
;; predictability of `windmove' and pack it into a single key binding, similar
;; to other-window.

;;; Code:

(require 'lib-util)

(lib-util/pkg ace-window
  :elpaca (:ref "77115afc1b0b9f633084cf7479c767988106c196")
  ;; 2023-05-20: I haven't used it for a long time.
  :disabled t
  :defer t

  :init
  ;; Do not wash-out colors from windows.
  (setq aw-background nil)

  ;; Set-up keys in the home row instead of the default [1-9].
  (setq aw-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l)))

(provide 'pkg-ace-window)
