;;; -*- lexical-binding: t; -*-

;;; Commentary:
;; Review on 2022-08-06. The completion list is extremely fast, VS Code like
;; speed. It kind of works, but there are issues:
;;
;; 1. Code completion appears in comment sections.
;;
;; 2. I couldn't find a quick way to set keybindings such as C-j and C-k that
;; play well with evil. To be fair, this is not the package's fault, but it's
;; also not well documented.
;;
;; 3. Certain candidates are not displayed in Typescript autocompletion, even
;; though the language server is configured just like VS Code, for example.
;;
;; 4. Lacks configurability. For example, elisp autocompletion can be configured
;; with a minimum threashold, but there's no general support for other
;; languages.

;;; Code:
(require 'use-package)

(my/package lsp-bridge
  :straight (:host github
             :repo "manateelazycat/lsp-bridge"
             :files (:defaults "*.py" "langserver" "acm"))
  :defer t

  :hook (after-init-hook . global-lsp-bridge-mode)

  :init
  ;; This is not working because evil shadows keybindings, like C-n and C-k.
  (general-def
    :keymaps '(acm-mode-map)
    "RET" #'newline
    "C-j" #'acm-select-next
    "C-k" #'acm-select-prev
    "C-n" #'acm-select-next
    "C-p" #'acm-select-prev)

  ;; This version of python should have epc installed (pip install epc).
  ;;
  ;; $ cd $HOME/.emacs.d/.local/cache
  ;; $ python3 -m venv venv
  ;; $ ./python3 -m pip install --upgrade pip
  ;; $ ./pip install epc
  ;;
  ;; $ npm install -g typescript typescript-language-server
  (setq lsp-bridge-python-command (concat my/cache-dir "venv/bin/python"))

  (setq lsp-bridge-epc-accept-process-timeout 150)
  (setq lsp-bridge-diagnostics-fetch-idle 1)
  (setq acm-candidate-match-function #'orderless-literal)

  (setq acm-icon-width 3)

  (setq acm-enable-doc nil)
  (setq acm-enable-icon t)
  (setq acm-enable-path nil)
  (setq acm-enable-search-words nil)
  (setq acm-enable-tempel nil)
  (setq acm-enable-yas nil)
  (setq acm-enable-english-helper nil)
  (setq acm-backend-lsp-enable-auto-import t)
  (setq acm-backend-elisp-min-length 2)
  (setq acm-backend-lsp-candidates-max-number 500)
  (setq lsp-bridge-enable-candidate-doc-preview nil)
  (setq lsp-bridge-enable-search-words nil)
  (setq lsp-bridge-enable-signature-help t)

  ;; ;; Debugging.
  (setq lsp-bridge-enable-log nil)
  (setq lsp-bridge-epc-debug nil)

  ;; If you got segfault error, please turn this option. Then LSP-Bridge will
  ;; start by gdb, please send new issue with *lsp-bridge* buffer content when
  ;; next crash.
  (setq lsp-bridge-enable-debug nil))

(provide 'pkg-lsp-bridge)
