;;; -*- lexical-binding: t; -*-

;;; Commentary:
;; Thesaurus front-end with pluggable backends.

;;; Code:

(lib-util/pkg synosaurus
  :elpaca (:ref "14d34fc92a77c3a916b4d58400424c44ae99cd81")
  :defer t
  :commands (synosaurus-lookup
             synosaurus-choose-and-insert
             synosaurus-choose-and-replace)
  :init
  ;; WordNet is an English offline thesaurus. Requires wordnet (wn) as a system
  ;; dependency.
  (setq synosaurus-backend #'synosaurus-backend-wordnet))

(provide 'pkg-synosaurus)
