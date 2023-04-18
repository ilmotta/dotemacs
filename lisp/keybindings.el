;;; -*- lexical-binding: t; -*-
;;; Code:

;;; Non mode-specific keybindings

(with-eval-after-load 'general
  (general-def
    :keymaps 'my/keys-mode-map
    :states 'normal
    :prefix my/leader
    "m i" #'imenu)

  (general-def
    :keymaps 'my/keys-mode-map
    "C-c z" #'global-text-scale-adjust)

  (general-def
    :keymaps '(my/keys-mode-map)
    :states '(normal visual insert emacs)
    :prefix my/leader
    :non-normal-prefix my/non-normal-prefix
    "x t t" #'pkg-theme/cycle-dark-light)

  (general-def
    :keymaps 'prog-mode-map
    :states 'normal
    :prefix my/local-leader
    "o" #'find-sibling-file)

  (general-def
    :keymaps 'my/keys-mode-map
    :states '(normal insert emacs visual)
    :prefix my/leader
    :non-normal-prefix "C-x"
    "f" #'pkg-emacs/file-command-map)

  (general-def
    :keymaps 'my/keys-mode-map
    :states 'normal
    :prefix my/leader
    "o c" #'calc
    "o m" #'proced
    "o p" #'list-processes)

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
    "s-'" #'pkg-window/split-window-below
    "s-L" #'tab-bar-switch-to-next-tab
    "s-H" #'tab-bar-switch-to-prev-tab))

;;; Keybindings for built-in packages

(with-eval-after-load 'general
  (general-def
    :states '(normal visual insert emacs)
    :prefix my/local-leader
    :non-normal-prefix my/non-normal-prefix
    :keymaps '(emacs-lisp-mode-map)
    ;; Code
    "c ="   #'lib-util/sort-up-sexp
    ;; Debug
    "d f"   #'edebug-defun
    ;; Eval
    "e :"   #'eval-expression
    "e b"   #'pkg-elisp-mode/eval-buffer
    "e e"   #'eval-defun
    "e f"   #'pkg-elisp-mode/eval-list-at-point
    "e l"   #'eval-last-sexp
    "e p :" #'pp-eval-expression
    "e p b" #'pp-buffer
    "e p p" #'pp-eval-last-sexp
    "e r"   #'eval-region
    "e 2"   #'pkg-elisp-mode/eval-defun-2nd-symbol
    ;; Help
    "h ."   #'helpful-at-point
    "h a a" #'apropos
    "h a d" #'apropos-documentation
    ;; Macroexpand
    "m :"   #'pp-macroexpand-expression
    "m m"   #'pp-macroexpand-last-sexp
    ;; Test
    "t b"   #'pkg-elisp-mode/run-file-tests
    "t p"   #'pkg-elisp-mode/run-project-tests)

  (general-def
    [remap dired-hide-details-mode] #'pkg-dired/hide-details-mode
    [remap xref-find-references] '(pkg-xref/find-references-dwim :properties (:jump t))
    [remap xref-find-definitions] '(pkg-xref/find-definitions :properties (:jump t)))

  (general-def
    :keymaps '(my/keys-mode-map)
    "s-<return>" #'tempo-complete-tag)

  (general-def
    :keymaps '(my/keys-mode-map)
    :predicate '(bound-and-true-p tempo-marks)
    "s-]" #'tempo-forward-mark
    "s-[" #'tempo-backward-mark)

  (general-def
    :keymaps 'override
    :prefix my/leader
    :states '(normal visual)
    "p" '(:keymap project-prefix-map :package project))

  (general-def
    :keymaps 'project-prefix-map
    ;; Leave "s" for the Search mnemonic, as I use other commands to open
    ;; shells.
    "s" nil
    "." #'lib-util/project-switch-to-dotfiles)

  (general-def
    :keymaps 'dired-mode-map
    "S-SPC" nil
    "S-SPC <return>" #'pkg-dired/transient)

  (general-def
    :keymaps 'eshell-mode-map
    :states '(emacs insert)
    "C-y" #'yank
    "C-a" #'eshell-bol
    "C-e" #'move-end-of-line
    "C-l" #'pkg-eshell/clear)

  (general-def
    :keymaps 'eshell-mode-map
    :states 'normal
    :prefix "m"
    "c c" #'pkg-eshell/change-line
    "y y" #'pkg-eshell/copy-dwim
    "y i" #'pkg-eshell/copy-input
    "y o" #'pkg-eshell/copy-output)

  (general-def
    :keymaps 'my/keys-mode-map
    :states '(visual emacs)
    "C-c G r" #'lib-media/google-tts-read-region
    "C-c G d" #'lib-media/google-tts-dictate-region)

  (general-def
    :keymaps 'my/keys-mode-map
    :states '(normal insert emacs visual)
    :prefix my/leader
    :non-normal-prefix "C-x"
    "w" #'pkg-emacs/window-command-map)

  (general-def
    :keymaps '(outline-mode-map outline-minor-mode-map)
    :states 'normal
    :predicate '(outline-on-heading-p)
    "C-j"   '(outline-next-visible-heading :properties (:jump t))
    "C-k"   '(outline-previous-visible-heading :properties (:jump t))
    "C-M-j" '(pkg-outline/next-hide-other-heading :properties (:jump t))
    "C-M-k" '(pkg-outline/previous-hide-other-heading :properties (:jump t)))

  (general-def
    :keymaps '(outline-mode-map outline-minor-mode-map)
    :states 'normal
    "z /" #'consult-outline
    "z b" #'outline-cycle-buffer
    "z j" '(outline-next-visible-heading :properties (:jump t))
    "z k" '(outline-previous-visible-heading :properties (:jump t))
    "z p" #'outline-hide-other
    "z u" '(outline-up-heading :properties (:repeat t :jump t)))

  (general-def
    :keymaps '(outline-mode-map outline-minor-mode-map)
    :states 'normal
    :predicate '(outline-on-heading-p)
    "z a" #'outline-toggle-children
    "z c" #'outline-hide-entry
    "z C" #'outline-hide-subtree
    "z d" '(pkg-outline/delete-tree :properties (:repeat t))
    "z J" '(outline-move-subtree-down :properties (:repeat t))
    "z K" '(outline-move-subtree-up :properties (:repeat t))
    "z o" #'outline-show-entry
    "z O" #'outline-show-subtree))

(provide 'keybindings)
