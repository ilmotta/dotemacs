;;; -*- lexical-binding: t; -*-

;;; Commentary:
;; Activate URLs and e-mail addresses. When this buffer-local minor mode is
;; enabled, it finds all the URLs in the buffer, highlights them, and turns them
;; into clickable buttons.

;;; Code:

(my/package goto-addr
  :straight (:type built-in)
  :defer t
  :hook (text-mode-hook . goto-address-mode)
  :hook (prog-mode-hook . goto-address-prog-mode)

  ;; NOTE: There's not keybinding because I prefer to use `embark-act' followed
  ;; by RET.
  )

(provide 'pkg-goto-addr)
