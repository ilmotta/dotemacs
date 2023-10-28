;;; -*- lexical-binding: t; -*-

;;; Commentary:
;; Interface to WordNet, an English offline thesaurus. Unlike `synosaurus', this
;; package will display lots of things other than simple lists of synonyms.

;;; Code:

(require 'lib-util)

(lib-util/pkg wordnut
  :elpaca (:ref "feac531404041855312c1a046bde7ea18c674915")
  :defer t
  :commands (wordnut-search wordnut-history-lookup)
  :init
  (when my/evil-p
    (general-def
      :keymaps 'wordnut-mode-map
      :states 'normal
      "RET"     #'wordnut-lookup-current-word
      "C-c C-u" #'outline-up-heading
      "C-j"     #'outline-forward-same-level
      "C-k"     #'outline-backward-same-level
      "g j"     #'outline-forward-same-level
      "g k"     #'outline-backward-same-level
      "H"       #'wordnut-history-lookup
      ">"       #'wordnut-history-forward
      "<"       #'wordnut-history-backward)))

(provide 'pkg-wordnut)
