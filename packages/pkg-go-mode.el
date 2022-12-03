;;; -*- lexical-binding: t; -*-

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

(my/package go-mode
  :straight t
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
