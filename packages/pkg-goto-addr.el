;;; -*- lexical-binding: t; -*-

;;; Commentary:

;; Activate URLs and e-mail addresses. When this buffer-local minor mode is
;; enabled, it finds all the URLs in the buffer, highlights them, and turns them
;; into clickable buttons.
;;
;; NOTE: There's no keybinding because I prefer to use `embark-act' followed by
;; RET.

;;; Code:

(my/package goto-addr
  :elpaca nil
  :hook (text-mode-hook . goto-address-mode)
  :hook (prog-mode-hook . goto-address-prog-mode))

(provide 'pkg-goto-addr)
