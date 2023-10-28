;;; -*- lexical-binding: t; -*-

;;; Commentary:
;; Provides a minor mode which displays current match and total matches
;; information in the mode-line in various search modes.

;;; Code:

(require 'lib-util)

(lib-util/pkg anzu
  :elpaca (:ref "5abb37455ea44fa401d5f4c1bdc58adb2448db67")
  :defer t
  :init
  (general-def
    [remap query-replace] #'anzu-query-replace
    [remap query-replace-regexp] #'anzu-query-replace-regexp))

(provide 'pkg-anzu)
