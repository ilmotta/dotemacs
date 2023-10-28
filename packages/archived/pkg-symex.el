;;; -*- lexical-binding: t; -*-

;;; Commentary:
;; Structural editing Lisp (and soon other programming languages via
;; tree-sitter).
;;
;; Update @ 2023-04-21: waiting for the 2.0 release.

(lib-util/pkg
  (symex :ref "9fe07170ff5370cea9e6edbf093cb1ea8f71e28b")
  :defer t

  :init
  ;; Reverse the default behavior, as it feels more natural in evil to press k
  ;; to go up (visually).
  (setq symex--user-evil-keyspec
        '(("j" . symex-go-up)
          ("k" . symex-go-down)
          ("C-j" . symex-climb-branch)
          ("C-k" . symex-descend-branch)
          ("M-j" . symex-goto-highest)
          ("M-k" . symex-goto-lowest)))

  (general-def
    :keymaps 'my/keys-mode-map
    "s-," #'symex-mode-interface)

  :config
  (setq symex-modal-backend 'evil)
  (symex-initialize))

(provide 'pkg-symex)
