;;; -*- lexical-binding: t; -*-

(require 'lib-util)

(defun my/translate-keys-p (key-from)
  "Returns whether conditional key translations should be active.
See make-conditional-key-translation function."
  (and
   ;; Only allow a non identity translation if we're beginning a Key Sequence.
   (equal key-from (this-command-keys))
   (or (evil-motion-state-p)
       (evil-normal-state-p)
       (evil-visual-state-p))))

(defun my/translate-key-maybe (key-from key-to)
  "Make a key translation such that if the `my/translate-keys-p' function
returns true, KEY-FROM translates to KEY-TO, else KEY-FROM
translates to itself. `my/translate-keys-p' takes KEY-FROM as an
argument."
  (define-key key-translation-map
              key-from
              (lambda (prompt)
                (if (my/translate-keys-p key-from)
                    key-to
                  key-from))))

(defun pkg-evil/setup-quick-cancel ()
  ;; I'm used to C-g, but pressing ESC is faster if I rebind CAPS LOCK to ESC.
  (let ((keymaps `(minibuffer-local-map
                   minibuffer-local-ns-map
                   minibuffer-local-completion-map
                   minibuffer-local-must-match-map
                   minibuffer-local-isearch-map
                   read-expression-map

                   ;; Allow cancelling y/n questions with escape.
                   ;; This is the keymap parent of `map-y-or-n-p'.
                   query-replace-map
                   )))
    (general-define-key
     :keymaps keymaps
     [escape] #'my/escape)))

(defun pkg-evil/setup ()
  (when my/evil-p
    (pkg-evil/setup-quick-cancel)
    (evil-set-initial-state 'calc-mode 'emacs)

    ;; Idea and code taken from https://www.emacswiki.org/emacs/Evil. I find "f"
    ;; to be even less disruptive and more convenient than pressing "g" on a
    ;; QWERTY layout.
    (my/translate-key-maybe (kbd "f") (kbd "C-x"))

    ;; Re-add evil commands to C-x so they're used by the key translation.
    (define-key evil-motion-state-map (kbd "C-x f") #'evil-find-char)

    (evil-mode +1)))

(lib-util/pkg evil
  :elpaca (:ref "0251080640e0da6f0eec2b7d8dd70e9c9b9915d7")
  :defer t
  :commands (evil-set-initial-state)

  ;; Disable evil-want-keybinding to avoid the warning "was set to nil but not
  ;; before loading evil".
  :custom (evil-want-keybinding nil)

  :init
  (add-hook 'elpaca-after-init-hook #'pkg-evil/setup -9999)

  (when my/evil-p
    (general-def
      :states 'insert
      ;; I don't use `evil-insert-digraph'.
      "C-k" nil)

    (general-def
      :states 'normal
      [remap evil-beginning-of-line] #'lib-util/smart-beginning-of-line
      "C-]" #'evil-goto-definition

      ;; Remove bindings conflicting with default Emacs behavior.
      "C-." nil ; `evil-repeat-pop'
      "M-." nil ; `evil-repeat-pop-next'
      "C-p" nil ; `evil-paste-pop'
      "C-n" nil ; `evil-paste-pop-next'
      ))

  (setq evil-undo-system 'undo-fu)
  (setq evil-want-visual-char-semi-exclusive t)
  (setq evil-ex-search-vim-style-regexp t)
  (setq evil-ex-visual-char-range t) ; Column range for ex commands
  (setq evil-mode-line-format 'nil)
  (setq evil-kbd-macro-suppress-motion-error t)
  (setq evil-move-beyond-eol t) ; Recommended by the evil-cleverparens package.

  ;; Let the modeline display the current modal state.
  (setq evil-emacs-state-message nil
        evil-insert-state-message nil
        evil-motion-state-message nil
        evil-normal-state-message nil
        evil-operator-state-message nil
        evil-replace-state-message nil
        evil-visual-state-message nil)

  (setq evil-emacs-state-tag "E"
        evil-insert-state-tag "I"
        evil-motion-state-tag "M"
        evil-normal-state-tag "N"
        evil-operator-state-tag "O"
        evil-replace-state-tag "R"
        evil-visual-state-tag "V")

  ;; More vim-like behavior.
  (setq evil-symbol-word-search t)
  (setq evil-want-C-u-scroll t) ; Moved the universal arg to <leader> u
  (setq evil-want-C-i-jump t)
  (setq evil-want-C-g-bindings nil)
  (setq evil-want-C-u-delete t)
  (setq evil-want-C-w-delete t)
  (setq evil-want-Y-yank-to-eol nil)
  (setq evil-want-abbrev-expand-on-insert-exit t)
  (setq evil-want-minibuffer nil)

  ;; Cursor appearance.
  (setq evil-default-cursor t)
  (setq evil-normal-state-cursor 'box)
  (setq evil-emacs-state-cursor  'bar)
  (setq evil-insert-state-cursor 'bar)
  (setq evil-visual-state-cursor 'hollow)

  ;; Only do highlighting in selected window so that Emacs has less work
  ;; to do highlighting them all.
  (setq evil-ex-interactive-search-highlight 'selected-window)

  (setq evil-split-window-below t)
  (setq evil-vsplit-window-right t)

  ;; Motions such as j and k navigate by visual lines (on the screen) rather
  ;; than "physical" lines (defined by newline characters).
  (setq evil-respect-visual-line-mode t)

  ;; Allows jumping back and forth between special buffers too.
  (setq evil--jumps-buffer-targets "\\*")

  (setq evil-jumps-max-length 250
        evil-jumps-cross-buffers t
        ;; evil--jumps-debug t
        ))

(provide 'pkg-evil)
