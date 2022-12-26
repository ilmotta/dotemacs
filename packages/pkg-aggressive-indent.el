;;; -*- lexical-binding: t; -*-

;;; Commentary:
;; Minor mode that keeps your code always indented. It re-indents after every
;; change, making it more reliable than =electric-indent-mode=, at least in Lisp
;; languages.

;;; Code:

(my/package
  (aggressive-indent :ref "f376cdc25de5c0f8c330f1e053557d95ca47a540")
  :defer t
  :init
  ;; Enable `aggressive-indent-mode' in all Lisp modes.
  (dolist (mode my/lisp-modes)
    (let ((hook (intern (concat (symbol-name mode) "-hook"))))
      (add-hook hook #'aggressive-indent-mode)))

  :config
  (global-aggressive-indent-mode -1))

(provide 'pkg-aggressive-indent)
