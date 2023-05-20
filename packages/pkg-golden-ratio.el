;;; -*- lexical-binding: t; -*-

;;; Commentary:
;; Automatic resizing of Emacs windows to the golden ratio.

;;; Code:

(my/package golden-ratio
  :elpaca (:ref "007911d8a431b72670f5fe5f0e5b4380c2777a31")
  :defer t
  :hook (golden-ratio-mode-hook . golden-ratio)
  :init
  (general-def
    :keymaps 'my/keys-mode-map
    :states 'normal
    :prefix my/leader
    "w g" #'golden-ratio-mode)

  ;; If you use a large screen and have very wide frames golden-ratio makes very
  ;; wide windows. This can be handled automatically by setting
  ;; golden-ratio-auto-scale to true. This does a good job of keeping windows at
  ;; a reasonable width regardless of how wide or narrow your frame size is.
  ;; This works well on my laptop regardless of which monitor or LCD I happen to
  ;; be using.
  (setq golden-ratio-auto-scale t))

(provide 'pkg-golden-ratio)
