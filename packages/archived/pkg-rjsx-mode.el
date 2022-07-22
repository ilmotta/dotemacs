;;; -*- lexical-binding: t; -*-
(require 'use-package)

;; https://github.com/felipeochoa/rjsx-mode derives from `js2-mode', extending
;; its parser to support JSX syntax according to the official spec. This means
;; you get all of the js2 features plus proper syntax checking and highlighting
;; of JSX code blocks. You can enable this mode on a per-project basis using
;; /directory locals/.
;;
;;   ((js-mode . ((mode . rjsx))))
(my/package rjsx-mode
  :straight t
  :disabled t
  :defer t
  :mode (rx "." (or "tsx" "jsx") string-end)
  :config
  (setq rjsx-print-debug-message nil)
  (with-eval-after-load 'company
    (pkg-company/set-backend 'rjsx-mode 'company-tide)))

(provide 'pkg-rjsx-mode)
