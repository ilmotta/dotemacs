;;; -*- lexical-binding: t; -*-

(defun pkg-kind-icon/reset-cache (&rest _args)
  (kind-icon-reset-cache))

;; SVG icons for completion backends.
(my/package kind-icon
  :elpaca (:ref "42d2a41874d5a61731556e53ba57547b4ef95342")
  :defer t

  :init
  (setq kind-icon-blend-background nil)
  (setq kind-icon-blend-frac 0.12)
  (setq kind-icon-default-style '(:padding 0 :stroke 0 :margin 0 :radius 0 :height 1.0 :scale 0.9))

  ;; Use text instead of icons, in HiDPI screens I couldn't get `kind-icon' to
  ;; play nicely. See https://github.com/jdtsmith/kind-icon/issues/22
  (when my/machine-matrix-p
    (setq kind-icon-use-icons nil))

  ;; `svg-lib' is a required dependency.
  (setq svg-lib-icons-dir (concat my/cache-dir "svg-lib/"))

  :config
  (with-eval-after-load 'corfu
    (setq kind-icon-default-face 'corfu-default)
    (add-to-list 'corfu-margin-formatters #'kind-icon-margin-formatter))

  ;; Without resetting the cache, icons will sometimes look way bigger/smaller.
  (advice-add #'load-theme :after #'pkg-kind-icon/reset-cache))

(provide 'pkg-kind-icon)
