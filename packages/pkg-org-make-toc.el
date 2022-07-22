;;; -*- lexical-binding: t; -*-

;;; Commentary:
;; This package makes it easy to have one or more customizable tables of
;; contents in Org files.

;;; Code:
(my/package org-make-toc
  :straight t
  :defer t
  :commands (org-make-toc
             org-make-toc-insert
             org-make-toc-set))

(provide 'pkg-org-make-toc)
