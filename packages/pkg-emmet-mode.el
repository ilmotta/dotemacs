;;; -*- lexical-binding: t; -*-

;;; Commentary:
;; https://github.com/smihica/emmet-mode is a minor mode providing support for
;; https://emmet.io/'s feature set is a plugin for many popular text editors
;; which greatly improves HTML & CSS workflow. Its
;; http://docs.emmet.io/abbreviations/ is inspired by CSS selectors.

;;; Code:
(my/package emmet-mode
  :elpaca (:ref "63b6932603184956b5ea8919036d2b307b48d7fd")
  :defer t
  :hook ((js-mode-hook
          typescript-tsx-mode-hook) . emmet-mode)
  :init
  (when (bound-and-true-p evil-mode)
    (general-def
      :keymaps 'emmet-mode-keymap
      "C-j" nil)))

(provide 'pkg-emmet-mode)
