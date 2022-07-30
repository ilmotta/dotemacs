;;; -*- lexical-binding: t; -*-

(defun pkg-emmet-mode/setup ()
  (when (bound-and-true-p js-jsx-syntax)
    (emmet-mode +1)))

;; https://github.com/smihica/emmet-mode is a minor mode providing support for
;; https://emmet.io/'s feature set is a plugin for many popular text editors
;; which greatly improves HTML & CSS workflow. Its
;; http://docs.emmet.io/abbreviations/ is inspired by CSS selectors.
(my/package emmet-mode
  :straight t
  :defer t
  :hook (js-mode-hook . pkg-emmet-mode/setup)
  :hook (typescript-tsx-mode-hook . emmet-mode)
  :init
  (when (bound-and-true-p evil-mode)
    (general-def
      :keymaps 'emmet-mode-keymap
      "C-j" nil)))

(provide 'pkg-emmet-mode)
