;;; -*- lexical-binding: t; -*-

;;; Code:

(defun pkg-ctrlf/recenter-after-default-search (&rest _args)
  (recenter-top-bottom))

;; An intuitive and efficient solution for single-buffer text search in
;; Emacs,replacing packages such as Isearch, Swiper, and helm-swoop.
(lib-util/pkg ctrlf
  :elpaca (:ref "9b4cf6c79a961f2bfbb949805aa300fcf1eb40a6")
  :defer t

  :init
  (general-def
    [remap evil-search-forward] #'ctrlf-forward-default
    [remap isearch-backward-regexp] #'ctrlf-backward-regexp
    [remap isearch-backward] #'ctrlf-backward-literal
    [remap isearch-forward-regexp] #'ctrlf-forward-regexp
    [remap isearch-forward] #'ctrlf-forward-default)

  ;; This is a workaround to force escape to go back to the original cursor
  ;; position.
  (general-def
    :keymaps 'ctrlf-minibuffer-mode-map
    [remap my/escape] #'ctrlf-cancel)

  (setq ctrlf-default-search-style 'literal)
  (setq ctrlf-auto-recenter t)
  (setq ctrlf-show-match-count-at-eol t)
  (setq ctrlf-highlight-current-line nil)

  :config
  ;; Avoids jumping up/down after the search ends.
  (advice-add #'ctrlf-forward-default :after #'pkg-ctrlf/recenter-after-default-search))

(provide 'pkg-ctrlf)
