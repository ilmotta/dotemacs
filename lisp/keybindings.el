;;; -*- lexical-binding: t; -*-
;;; Code:
;;; Files

(general-def
  :keymaps 'my/keys-mode-map
  :states 'normal
  :prefix my/local-leader
  "o" #'find-sibling-file)

;;; Process

(general-def
  :keymaps 'my/keys-mode-map
  :states 'normal
  :prefix my/leader
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

;;; Text-to-speach

(general-def
  :keymaps 'my/keys-mode-map
  :states '(visual emacs)
  "C-c G r" #'lib-media/google-tts-read-region
  "C-c G d" #'lib-media/google-tts-dictate-region)

;;; Window

(general-def
  :keymaps 'my/keys-mode-map
  :states '(normal insert emacs visual)
  :prefix my/leader
  :non-normal-prefix "C-x"
  "w" #'pkg-emacs/window-command-map)

;;; Super shortcuts

(general-def
  :keymaps 'my/keys-mode-map
  "s-f" #'project-find-file
  "s-m" #'magit-status
  "s-i" #'execute-extended-command
  "s-n" #'pkg-org-roam/node-find
  "s-p" #'project-switch-project
  "s-s" #'save-buffer
  "s-u" #'switch-to-buffer
  "s-U" #'project-switch-to-buffer
  "s-w" #'delete-window
  "s-;" #'pkg-window/split-window-right
  "s-'" #'pkg-window/split-window-below)

;;; UI

(general-def
  :keymaps 'my/keys-mode-map
  "C-c z" #'global-text-scale-adjust)

(provide 'keybindings)
