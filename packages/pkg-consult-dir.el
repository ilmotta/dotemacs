;;; -*- lexical-binding: t; -*-

;;; Commentary:
;; Consult-dir allows you to easily insert directory paths into the minibuffer
;; prompt in Emacs. Think of it like the shell tools autojump, fasd or z but for
;; Emacs.

;;; Code:

(my/package
  (consult-dir :ref "ed8f0874d26f10f5c5b181ab9f2cf4107df8a0eb")
  :defer t
  :init
  (general-def
    :keymaps 'minibuffer-mode-map
    "s-d" #'consult-dir))

(provide 'pkg-consult-dir)
