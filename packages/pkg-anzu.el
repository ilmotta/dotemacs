;;; -*- lexical-binding: t; -*-

;;; Commentary:
;; Provides a minor mode which displays current match and total matches
;; information in the mode-line in various search modes.

;;; Code:

(my/package anzu
  :straight t
  :defer t
  :init
  (general-def
    [remap query-replace] #'anzu-query-replace
    [remap query-replace-regexp] #'anzu-query-replace-regexp))

(provide 'pkg-anzu)
