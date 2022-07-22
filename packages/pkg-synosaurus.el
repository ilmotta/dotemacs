;;; -*- lexical-binding: t; -*-

;;; Commentary:
;; Thesaurus front-end with pluggable backends.

;;; Code:

(my/package synosaurus
  :straight t
  :defer t
  :commands (synosaurus-lookup
             synosaurus-choose-and-insert
             synosaurus-choose-and-replace)
  :init
  ;; WordNet is an English offline thesaurus. Requires wordnet (wn) as a system
  ;; dependency.
  (setq synosaurus-backend #'synosaurus-backend-wordnet))

(provide 'pkg-synosaurus)
