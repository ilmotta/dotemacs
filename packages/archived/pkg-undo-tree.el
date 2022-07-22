;;; -*- lexical-binding: t; -*-
(require 'use-package)

(defun pkg-undo-tree/reset-line-spacing ()
  (setq-local line-spacing 0))

(my/package undo-tree
  :straight t
  ;; Weird behavior sometimes, experimenting with undo-fu instead (2022-04-03).
  :disabled t
  :demand t
  :hook (undo-tree-visualizer-mode-hook . pkg-undo-tree/reset-line-spacing)

  :general
  (:keymaps 'undo-tree-visualizer-mode-map
   [remap evil-find-char-to] #'undo-tree-visualizer-toggle-timestamps
   [remap evil-quickscope-find-char-to] #'undo-tree-visualizer-toggle-timestamps)

  (:keymaps 'undo-tree-map
   ;; I want to use this keybinding for line search.
   "C-/" nil)

  :config
  (setq undo-tree-enable-undo-in-region t
        undo-tree-visualizer-diff nil)

  (global-undo-tree-mode +1))

(provide 'pkg-undo-tree)
