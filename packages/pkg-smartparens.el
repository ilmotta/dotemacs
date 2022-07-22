;;; -*- lexical-binding: t; -*-

;;; Commentary:
;; Like paredit, but with less bugs.

;;; Code:

(my/package smartparens
  :straight t
  :defer t

  :init
  ;; Enable `smartparens-mode' in all Lisp modes.
  (dolist (hook my/lisp-modes-hooks)
    (add-hook hook #'smartparens-strict-mode))

  ;; This should be true by default, so that the behaviour is controlled by
  ;; subword-mode.
  (setq sp-use-subword t)

  ;; Avoid unbalanced parentheses in insert mode.
  (setq sp-autoskip-closing-pair 'always)
  (setq sp-autoskip-opening-pair t)

  (setq sp-autodelete-closing-pair t
        sp-autodelete-pair t
        sp-autodelete-opening-pair t)

  ;; In Lisps, I prefer to rely on `aggressive-indent-mode'.
  (setq sp-navigate-reindent-after-up nil
        sp-navigate-reindent-after-up-in-string nil)

  ;; Turning this option on might have irreversible consequences on the buffer's
  ;; undo information and in some cases might remove important information.
  (setq sp-undo-pairs-separately nil)

  ;; String that is inserted after calling `sp-comment'.
  (setq sp-comment-string '(((clojure-mode emacs-lisp-mode) . ";; ")))

  (setq sp-show-pair-delay 0.05
        sp-show-pair-from-inside t)

  :config
  ;; Automatically configure smartparens for various modes.
  (require 'smartparens-config)

  ;; Silence some harmless but annoying echo-area spam.
  (dolist (key '(:unmatched-expression :no-matching-tag))
    (setf (alist-get key sp-message-alist) nil)))

(provide 'pkg-smartparens)
