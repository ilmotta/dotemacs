;;; -*- lexical-binding: t; -*-

(my/package popper
  :straight t
  :defer t
  :hook (after-init-hook . popper-mode)
  :init
  (general-def
    :keymaps '(my/keys-mode-map)
    "C-;" #'popper-toggle-latest
    "C-'" #'popper-cycle)

  ;; Leave display control to `display-buffer-alist'.
  (setq popper-display-control nil)

  (setq popper-mode-line "")
  (setq popper-group-function #'popper-group-by-project)

  ;; List of buffers to treat as popups.
  ;;
  ;; Note: because of how some shell buffers are initialized in Emacs, you may
  ;; need to supply both the name and major mode to match them consistently.
  (setq popper-reference-buffers
        `(;; Eshell as a popup.
          ,(rx line-start "*eshell" (zero-or-more not-newline) line-end)
          eshell-mode

          ;; Vterm as a popup.
          ,(rx line-start "*vterm" (zero-or-more not-newline) line-end)
          vterm-mode

          ;; Reference by mode.
          compilation-mode
          flycheck-error-list-mode
          messages-buffer-mode
          org-roam-mode

          ,(rx line-start (literal shell-command-buffer-name-async) line-end))))

(provide 'pkg-popper)
