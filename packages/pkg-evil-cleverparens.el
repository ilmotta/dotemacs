;;; -*- lexical-binding: t; -*-

(defun pkg-evil-cleverparens/setup-modes ()
  (dolist (mode-hook my/lisp-modes-hooks)
    (add-hook mode-hook #'evil-cleverparens-mode)))

(defun pkg-evil-cleverparens/add-command-props ()
  (evil-add-command-properties #'evil-cp-backward-up-sexp :jump t)
  (evil-add-command-properties #'evil-cp-up-sexp :jump t))

(lib-util/pkg evil-cleverparens
  :elpaca (:ref "22aa03d0f50aa70ae08fbe8765a88f5020afa635")
  :defer t

  ;; Command properties must be added after `evil-cleverparens-mode'is enabled.
  :hook (evil-cleverparens-enabled-hook . pkg-evil-cleverparens/add-command-props)

  :init
  ;; Moving the mouse in the terminal triggers this keybinding.
  (unless (display-graphic-p)
    (general-def
      :keymaps 'evil-cleverparens-mode-map
      :states 'normal
      "M-[" nil))

  (when (equal "M" my/windmove-modifier)
    (general-def
      :states '(normal visual)
      :keymaps 'evil-cleverparens-mode-map
      "M-l" nil ; evil-cp-end-of-defun
      "M-h" nil ; evil-cp-beginning-of-defun
      "M-j" nil ; evil-cp-drag-forward
      "M-k" nil ; evil-cp-drag-backward
      ))

  (general-def
    :states '(normal visual)
    :keymaps 'evil-cleverparens-mode-map
    "M-J" #'evil-cp-drag-forward
    "M-K" #'evil-cp-drag-backward)

  (setq evil-cleverparens-swap-move-by-word-and-symbol nil)
  (setq evil-cleverparens-move-skip-delimiters t)

  (with-eval-after-load 'evil
    (pkg-evil-cleverparens/setup-modes)))

(provide 'pkg-evil-cleverparens)
