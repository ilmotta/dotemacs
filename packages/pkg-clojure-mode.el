;;; -*- lexical-binding: t; -*-

;;; Variables

(defvar-local pkg-clojure/tempo-tags nil)

;;; Functions

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
  (cond ((bound-and-true-p cider-mode) ; Prefer CIDER to find definitions
         (let ((keyword (cider-symbol-at-point 'look-back)))
           (cond ((and (fboundp 're-frame-jump-to-reg)
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
                 (:default (cider-find-dwim keyword)))
           (recenter)))

        ;; Fallback to LSP Clojure
        ((bound-and-true-p lsp-mode)
         (call-interactively #'lsp-find-definition)
         (recenter))

        (:default
         (message "You need to enable CIDER and/or LSP Clojure to jump to a definition."))))

(defun pkg-clojure/tempo-setup ()
  (tempo-define-template
   "clojure-defn"
   '("(defn " p
     n> "[" p "]"
     n> p ")")
   "defn"
   "Insert defn expression."
   'pkg-clojure/tempo-tags)

  (tempo-use-tag-list 'pkg-clojure/tempo-tags))

(defun pkg-clojure/setup-sibling-rules ()
  (setq-local find-sibling-rules
              (list
               ;; Clojure(Script) src -> test
               (list (rx (group (+ (not "/")))
                         "." (group (or "clj" "cljs"))
                         string-end)
                     (rx (regex "\\1")
                         "_test." (regex "\\2")
                         string-end))

               ;; Clojure(Script) test -> src
               (list (rx (group (+ (not "/")))
                         "_test." (group (or "clj" "cljs"))
                         string-end)
                     (rx (regex "\\1")
                         "." (regex "\\2")
                         string-end))

               ;; Clojure (Java standard) src -> test
               (list (rx "src/"
                         (group (* not-newline))
                         "." (group (or "clj" "cljs"))
                         string-end)
                     (rx "test/"
                         (regex "\\1")
                         "_test." (regex "\\2")
                         string-end))

               ;; Clojure (Java standard) test -> src
               (list (rx "test/"
                         (group (* not-newline))
                         "_test." (group (or "clj" "cljs"))
                         string-end)
                     (rx "src/"
                         (regex "\\1")
                         "." (regex "\\2")
                         string-end)))))

;;; Package

(my/package
  (clojure-mode :ref "3453cd229b412227aaffd1dc2870fa8fa213c5b1")
  :defer t

  :hook (clojure-mode-hook . pkg-clojure/setup-sibling-rules)
  :hook (clojure-mode-hook . outline-minor-mode)
  :hook (outline-minor-mode-hook . pkg-clojure-mode/outline-minor-mode-h)
  :hook (clojure-mode-hook . pkg-clojure/tempo-setup)

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
  (define-clojure-indent (wait-for :defn))
  (define-clojure-indent (letsubs 1))

  (with-eval-after-load 'flycheck
    (require 'flycheck-clj-kondo nil 'noerror))

  (with-eval-after-load 'smartparens
    (require 'smartparens-clojure)))

(provide 'pkg-clojure-mode)
