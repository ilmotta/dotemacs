;;; -*- lexical-binding: t; -*-

;;; Commentary:
;; Automatic resizing of Emacs windows to the golden ratio.

;;; Code:

(require 'lib-util)

(lib-util/pkg golden-ratio
  :ensure (:ref "007911d8a431b72670f5fe5f0e5b4380c2777a31")
  :defer t
  :hook (golden-ratio-mode-hook . golden-ratio)
  :init
  (general-def
    :keymaps 'my/keys-mode-map
    :states 'normal
    :prefix my/leader
    "w g" #'golden-ratio-mode)

  ;; Manually adjusting the factor gives better results in my screens.
  (setq golden-ratio-adjust-factor 1.0
        golden-ratio-auto-scale nil))

(provide 'pkg-golden-ratio)
