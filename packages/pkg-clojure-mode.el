;;; -*- lexical-binding: t; -*-

(defun pkg-clojure-mode/outline-minor-mode-h ()
  (when (derived-mode-p 'clojure-mode)
    (setq-local outline-regexp my/outline-regex-lisp)))

(defun pkg-clojure-mode/find-var ()
  "Smart find var at point.

If CIDER mode is enabled then it checks if it's a ClojureScript
file and call `re-frame-jump-to-reg' if symbol at point is a
keyword, otherwise use normal `cider-find-dwim'. In case CIDER
mode is not enabled it tries to use LSP to find the definition."
  (interactive)
  (cond
    ;; Prefer CIDER to find definitions
    ((bound-and-true-p cider-mode)
     (let* ((keyword (cider-symbol-at-point 'look-back)))
       (cond
         ((and (fboundp 're-frame-jump-to-reg)
               (derived-mode-p 'clojurescript-mode)
               (or (string-match-p (rx line-start
                                       (repeat 1 2 ":")
                                       (group (one-or-more (not "/"))) "/"
                                       (group (one-or-more not-newline))
                                       line-end)
                                   keyword)
                   (string-match-p (rx line-start
                                       (repeat 1 2 ":")
                                       (group (one-or-more (not "/")))
                                       line-end)
                                   keyword)))
          (re-frame-jump-to-reg))
         (t (cider-find-dwim keyword)))
       (recenter)))

    ;; Fallback to LSP Clojure
    ((bound-and-true-p lsp-mode)
     (call-interactively #'lsp-find-definition)
     (recenter))

    (t (message "You need to enable CIDER and/or LSP Clojure to jump to a definition."))))

;;; Package

(my/package clojure-mode
  :straight t
  :defer t

  :hook (clojure-mode-hook . outline-minor-mode)
  :hook (outline-minor-mode-hook . pkg-clojure-mode/outline-minor-mode-h)

  :init
  (general-def
    :keymaps 'clojure-mode-map
    :states 'insert
    "M-a" #'sp-forward-sexp)

  (general-def
    :keymaps 'clojure-mode-map
    :states '(normal insert)
    "M-." #'pkg-clojure-mode/find-var
    "M->" #'cider-find-dwim-other-window)

  (my/general-mode-def
    :keymaps 'clojure-mode-map
    "c =" #'lib-util/sort-up-sexp)

  ;; Eval top level forms inside comment forms instead of the comment form
  ;; itself (experimental).
  (setq clojure-toplevel-inside-comment-form t)

  :config
  ;; Indentation settings
  (define-clojure-indent (prop/for-all 1))
  (define-clojure-indent (>defn :defn))
  (define-clojure-indent (async :defn))
  (define-clojure-indent (match :defn))
  (define-clojure-indent (letsubs 1))

  (with-eval-after-load 'flycheck
    (require 'flycheck-clj-kondo nil 'noerror))

  (with-eval-after-load 'smartparens
    (require 'smartparens-clojure)))

(provide 'pkg-clojure-mode)
