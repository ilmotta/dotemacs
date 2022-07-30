;;; -*- lexical-binding: t; -*-
;;; Code:
;;; Process

(general-def
  :keymaps 'my/keys-mode-map
  :states '(normal insert emacs visual)
  :prefix my/leader
  :non-normal-prefix my/non-normal-prefix
  "o m" #'proced
  "o p" #'list-processes)

;;; Buffer/file

(general-def
  :keymaps 'my/keys-mode-map
  :states '(normal insert emacs visual)
  :prefix my/leader
  :non-normal-prefix "C-x"
  "f" #'pkg-emacs/file-command-map)

;;; Calculator

(general-def
  :keymaps 'override
  :states 'normal
  :prefix my/leader
  "o c" #'calc)

;;; Eshell

(general-def
  :keymaps 'eshell-mode-map
  :states '(emacs insert)
  "C-y" #'yank
  "C-a" #'eshell-bol
  "C-e" #'move-end-of-line
  "C-l" #'pkg-eshell/clear
  "C-j" #'eshell-next-matching-input-from-input
  "C-k" #'eshell-previous-matching-input-from-input)

(general-def
  :keymaps 'eshell-mode-map
  :states 'normal
  :prefix "m"
  "c c" #'pkg-eshell/change-line
  "y y" #'pkg-eshell/copy-dwim
  "y i" #'pkg-eshell/copy-input
  "y o" #'pkg-eshell/copy-output)

(general-def
  :keymaps 'override
  :states 'normal
  :prefix my/leader
  "o s ." #'pkg-eshell/here
  "o s v" #'pkg-eshell/split-right
  "o s s" #'pkg-eshell/split-below)

(general-def
  :keymaps 'override
  :states '(insert normal)
  "C-'" #'pkg-eshell/project-toggle
  "C-<dead-acute>" #'pkg-eshell/project-toggle)

;;; Project

(general-def
  :keymaps 'override
  :prefix my/leader
  :states '(normal visual)
  "p" '(:keymap project-prefix-map :package project))

;;; Text-to-speach

(general-def
  :keymaps 'my/keys-mode-map
  :states '(visual emacs)
  "C-c G r" #'lib-media/google-tts-read-region
  "C-c G d" #'lib-media/google-tts-dictate-region)

;;; Xref

(general-def
  [remap xref-find-references] #'pkg-xref/find-references-dwim
  [remap xref-find-definitions] #'pkg-xref/find-definitions)

;;; Window

(general-def
  :keymaps 'my/keys-mode-map
  :states '(normal insert emacs visual)
  :prefix my/leader
  :non-normal-prefix "C-x"
  "w" #'pkg-emacs/window-command-map)

(provide 'keybindings)
;;; Super shortcuts

(general-def
  :keymaps 'my/keys-mode-map
  "s-f" #'project-find-file
  "s-g" #'magit-status
  "s-i" #'execute-extended-command
  "s-n" #'org-roam-node-find
  "s-p" #'project-switch-project
  "s-s" #'save-buffer
  "s-u" #'switch-to-buffer
  "s-U" #'project-switch-to-buffer
  "s-w" #'delete-window)
