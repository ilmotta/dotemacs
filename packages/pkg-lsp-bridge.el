;;; -*- lexical-binding: t; -*-

;;; Commentary:
;; Review on 2023-10-28. The completion list is extremely fast, VS Code like
;; speed. It kind of works, but there are issues, and I didn't like that it
;; doesn't leverage core Emacs libs.
;;
;; Showing refereces pops up a buffer that messes up all windows, and I couldn't
;; find ways to customize it.
;;
;; Didn't like that I can't customize the completion system to only show up when
;; I press tab. The completion popup is too distracting.

;;; Code:

(require 'lib-util)

(lib-util/pkg lsp-bridge
  :ensure (:ref "4e751899c49f83b0bd03b2f564972fbca839137b"
           :host github
           :repo "manateelazycat/lsp-bridge"
           :files (:defaults
                   "*.py"
                   "langserver/*.json"
                   "multiserver/*.json"
                   "resources/*.*"
                   "acm/*.el"
                   "acm/icons/*.svg")
           :build (:not compile))
  :disabled t

  :defer t

  ;; Prefer to enable the mode per buffer using a dir locals file because
  ;; lsp-bridge is too opinionated.
  ;;
  ;; :hook (elpaca-after-init-hook . global-lsp-bridge-mode)

  :init
  ;; lsp-bridge requires some Python libraries.
  ;;
  ;;   $ cd $HOME/.emacs.d/.local/cache
  ;;   $ virtualenv venv
  ;;   $ source venv/bin/activate
  ;;   $ pip install epc orjson sexpdata six paramiko rapidfuzz
  ;;
  (setq lsp-bridge-python-command (file-name-concat my/cache-dir "venv" "bin" "python"))
  (setq acm-icon-dir (file-name-concat elpaca-directory "builds" "lsp-bridge"))

  (setq lsp-bridge-diagnostic-display-errors-delay 0.25)
  (setq lsp-bridge-diagnostic-tooltip-border-width 20)
  (setq lsp-bridge-enable-auto-format-code nil)
  (setq lsp-bridge-enable-completion-in-minibuffer nil)
  (setq lsp-bridge-enable-diagnostics nil)
  (setq lsp-bridge-enable-inlay-hint t)
  (setq lsp-bridge-enable-org-babel nil)
  (setq lsp-bridge-enable-search-words nil)
  (setq lsp-bridge-enable-signature-help t)

  ;; This is not working because evil shadows keybindings, like C-n and C-k.
  (general-def
    :keymaps 'acm-mode-map
    "RET" #'newline
    "C-j" #'acm-select-next
    "C-k" #'acm-select-prev)

  (my/general-mode-def
    :keymaps '(clojure-mode-map)
    "c f d" #'lsp-bridge-find-def
    "c f i" #'lsp-bridge-find-impl
    "c f r" #'lsp-bridge-find-references
    "c r"   #'lsp-bridge-rename
    "c s"   #'lsp-bridge-workspace-list-symbols
    "h ."   #'lsp-bridge-popup-documentation)

  (setq lsp-bridge-epc-accept-process-timeout 150)
  (setq acm-candidate-match-function #'orderless-literal)

  (setq acm-backend-lsp-candidates-max-number 50)
  (setq acm-backend-lsp-enable-auto-import t)
  (setq acm-enable-doc nil)
  (setq acm-enable-icon t)
  (setq acm-enable-path nil)
  (setq acm-enable-tabnine nil)
  (setq acm-enable-telega nil)
  (setq acm-enable-tempel nil)
  (setq acm-enable-yas nil)
  (setq acm-icon-width 2)

  ;; ;; Debugging.
  (setq lsp-bridge-enable-log nil)
  (setq lsp-bridge-epc-debug nil)

  ;; If you got segfault error, please turn this option. Then LSP-Bridge will
  ;; start by gdb, please send new issue with *lsp-bridge* buffer content when
  ;; next crash.
  (setq lsp-bridge-enable-debug nil))

(provide 'pkg-lsp-bridge)
