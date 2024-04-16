;;; -*- lexical-binding: t; -*-

;;; Commentary:
;; EAT - Emulate a Terminal. It can run most (if not all) full-screen terminal
;; programs, including Emacs.

;;; Code:

(lib-util/pkg eat
  ;; Disabled because I see lots of glitches.
  :disabled t
  :elpaca (:fetcher codeberg
           :repo "akib/emacs-eat"
           :ref "3a6f418f55d183b9d86f99c140caed4ba3d44f93"
           :files ("*.el" "term/*.el" "*.texi"
                   "*.ti" "terminfo/e/*"
                   "terminfo/65/*"
                   "integration/*"
                   (:exclude ".dir-locals.el" "*-tests.el")))
  :defer t)

(provide 'pkg-eat)
