;;; -*- lexical-binding: t; -*-

;;; Commentary:
;; Deft is an Emacs mode for quickly browsing, filtering, and editing
;; directories of plain text notes, inspired by Notational Velocity.

;;; Code:
(require 'use-package)

(use-package deft
  :straight t
  :defer t

  ;; Kept here for reference, but I didn't find deft very helpful or even capable.
  :disabled t

  :init
  (setq deft-directory org-directory)
  (setq deft-recursive t)
  (setq deft-strip-summary-regexp ":PROPERTIES:\n\\(.+\n\\)+:END:\n")
  (setq deft-use-filename-as-title t)

  (general-def
    :keymaps 'override
    :states 'normal
    :prefix my/leader
    "n /" #'deft))

(provide 'pkg-deft)
