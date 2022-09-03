;;; -*- lexical-binding: t; -*-

;;; Commentary:
;; Tempo is a powerful snippet expansion system. For examples and documentation:
;;
;; - https://www.emacswiki.org/emacs/TempoSnippets
;; - http://www.lysator.liu.se/~davidk/elisp/tempo-examples.html

;;; Code:

(defvar-local pkg-tempo/tags-emacs-lisp nil)

(defun pkg-tempo/setup-emacs-lisp ()
  (tempo-define-template
   "emacs-lisp-defun"
   '("(defun " p " (" p ")" n>
     "\"" p "\""
     n> r ")")
   "defun"
   "Insert a defun expression."
   'pkg-tempo/tags-emacs-lisp)

  (tempo-use-tag-list 'pkg-tempo/tags-emacs-lisp))

(my/package tempo
  :straight (:type built-in)
  :defer t
  :init

  :hook (emacs-lisp-mode-hook . pkg-tempo/setup-emacs-lisp)

  :init
  (general-def
    :keymaps '(my/keys-mode-map)
    "s-<return>" #'tempo-complete-tag)

  (general-def
    :keymaps '(my/keys-mode-map)
    :predicate '(bound-and-true-p tempo-marks)
    "s-]" #'tempo-forward-mark
    "s-[" #'tempo-backward-mark)

  ;; Catch-all default regex.
  (setq-default tempo-match-finder (rx line-start
                                       (group (one-or-more not-newline))
                                       point)))

(provide 'pkg-tempo)
