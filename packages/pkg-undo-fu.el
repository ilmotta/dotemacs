;;; -*- lexical-binding: t; -*-

;;; Commentary:
;; Undo-Fu was written to be a simpler alternative as Undo Tree had long
;; standing unresolved bugs at the time of writing.

;;; Code:

(my/package undo-fu
  :elpaca (:host github
           :repo "emacsmirror/undo-fu"
           :ref "0e22308de8337a9291ddd589edae167d458fbe77")
  :demand t
  :init
  (setq undo-fu-allow-undo-in-region t)
  (setq undo-fu-ignore-keyboard-quit nil))

(provide 'pkg-undo-fu)
