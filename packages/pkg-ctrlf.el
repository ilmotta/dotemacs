;;; -*- lexical-binding: t; -*-

(defun pkg-ctrlf/recenter-after-default-search (&rest _args)
  (recenter-top-bottom))

;; An intuitive and efficient solution for single-buffer text search in
;; Emacs,replacing packages such as Isearch, Swiper, and helm-swoop.
(my/package ctrlf
  :straight t
  :defer t

  :init
  (general-def
    [remap evil-search-forward] #'ctrlf-forward-default
    [remap isearch-backward-regexp] #'ctrlf-backward-regexp
    [remap isearch-backward] #'ctrlf-backward-literal
    [remap isearch-forward-regexp] #'ctrlf-forward-regexp
    [remap isearch-forward] #'ctrlf-forward-default)

  ;; This is a workaround to fix an issue where C-g doesn't restore the original
  ;; cursor position.
  ;;
  ;; Issue: https://github.com/raxod502/ctrlf/issues/110
  (general-def
    :keymaps 'ctrlf-minibuffer-mode-map
    [remap abort-minibuffers] #'ctrlf-cancel)

  (setq ctrlf-default-search-style 'literal)
  (setq ctrlf-auto-recenter t)
  (setq ctrlf-show-match-count-at-eol t)
  (setq ctrlf-highlight-current-line nil)

  :config
  ;; Avoids jumping up/down after the search ends.
  (advice-add #'ctrlf-forward-default :after #'pkg-ctrlf/recenter-after-default-search))

(provide 'pkg-ctrlf)
