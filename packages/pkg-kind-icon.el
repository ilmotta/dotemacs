;;; -*- lexical-binding: t; -*-

(defun pkg-kind-icon/reset-cache (&rest _args)
  (kind-icon-reset-cache))

;; SVG icons for completion backends.
(my/package kind-icon
  :straight (:host github :repo "jdtsmith/kind-icon")
  :defer t

  :init
  (setq kind-icon-blend-background nil)
  (setq kind-icon-blend-frac 0.12)
  (setq kind-icon-default-style '(:padding 0 :stroke 0 :margin 0 :radius 0 :height 1.0 :scale 0.9))
  (setq kind-icon-use-icons t)

  ;; `svg-lib' is a required dependency.
  (setq svg-lib-icons-dir (concat my/cache-dir "svg-lib/"))

  :config
  (with-eval-after-load 'corfu
    (setq kind-icon-default-face 'corfu-default)
    (add-to-list 'corfu-margin-formatters #'kind-icon-margin-formatter))

  ;; Without resetting the cache, icons will sometimes look way bigger/smaller.
  (advice-add #'load-theme :after #'pkg-kind-icon/reset-cache)
  (advice-add #'default-text-scale-reset :after #'pkg-kind-icon/reset-cache)
  (advice-add #'default-text-scale-increase :after #'pkg-kind-icon/reset-cache)
  (advice-add #'default-text-scale-decrease :after #'pkg-kind-icon/reset-cache))

(provide 'pkg-kind-icon)
