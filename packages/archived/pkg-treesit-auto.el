;;; -*- lexical-binding: t; -*-

;;; Commentary:
;; Automatically pick between TreeSitter and default major modes in Emacs 29+.

;;; Code:

(defun pkg-treesit-auto/enable-a (&rest _r)
  (treesit-auto-apply-remap))

(my/package
  (treesit-auto :ref "bac3b9d1d61a4d759f87c80de7be3b808d19cbf6"
                :fetcher github
                :repo "renzmann/treesit-auto")
  :demand t

  :init
  (setq treesit-auto-install 'prompt)

  :config
  (add-to-list 'treesit-auto-fallback-alist
               '(bash-ts-mode . sh-mode))

  (advice-add 'treesit-install-language-grammar
              :after #'pkg-treesit-auto/enable-a)

  (treesit-auto-apply-remap))

(provide 'pkg-treesit-auto)
