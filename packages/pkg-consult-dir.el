;;; -*- lexical-binding: t; -*-

;;; Commentary:
;; Consult-dir allows you to easily insert directory paths into the minibuffer
;; prompt in Emacs. Think of it like the shell tools autojump, fasd or z but for
;; Emacs.

;;; Code:

(my/package consult-dir
  :straight t
  :defer t
  :init
  (general-def
    :keymaps 'minibuffer-mode-map
    "s-d" #'consult-dir))

(provide 'pkg-consult-dir)
