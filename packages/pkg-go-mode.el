;;; -*- lexical-binding: t; -*-

;;; Commentary:
;; As of today @ 2023-01-06, `go-mode' works better than `go-ts-mode'
;; (indentation being one important example). But if you still want to use
;; `go-ts-mode', here's how you can use it:
;;
;;    (add-to-list 'auto-mode-alist
;;                 (cons (rx ".go" string-end)
;;                       'go-ts-mode))
;;
;;    (add-hook 'go-ts-mode-hook #'pkg-go/setup-sibling-rules)
;;    (add-hook 'go-ts-mode-hook #'electric-pair-local-mode)

;;; Code:

(defun pkg-go/setup-sibling-rules ()
  (setq-local find-sibling-rules
              (list
               ;; Go src -> test
               (list (rx (group (+ (not "/")))
                         ".go"
                         string-end)
                     (rx (regex "\\1")
                         "_test.go"
                         string-end))

               ;; Go test -> src
               (list (rx (group (+ (not "/")))
                         "_test"
                         ".go"
                         string-end)
                     (rx (regex "\\1")
                         ".go"
                         string-end)))))

(my/package
  (go-mode :ref "166dfb1e090233c4609a50c2ec9f57f113c1da72")
  :defer t

  :hook (go-mode-hook . pkg-go/setup-sibling-rules)
  :hook (go-mode-hook . electric-pair-local-mode)

  :init
  (my/general-mode-def
    :keymaps '(go-mode-map)
    "g a" '(go-goto-arguments :properties (:jump t))
    "g d" '(go-goto-docstring :properties (:jump t))
    "g f" '(go-goto-function-name :properties (:jump t))
    "g i" '(go-goto-imports :properties (:jump t))
    "h ." '(godoc-at-point :properties (:jump t))
    "i a" #'go-import-add
    "i c" #'go-remove-unused-imports))

(provide 'pkg-go-mode)
