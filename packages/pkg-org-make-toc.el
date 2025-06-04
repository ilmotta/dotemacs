;;; -*- lexical-binding: t; -*-

;;; Commentary:
;; This package makes it easy to have one or more customizable tables of
;; contents in Org files.

;;; Code:

(require 'lib-util)

(lib-util/pkg org-make-toc
  :ensure (:ref "26fbd6a7e1e7f8e473fe3a5f74faec715c3a05aa")
  :defer t
  :commands (org-make-toc
             org-make-toc-insert
             org-make-toc-set))

(provide 'pkg-org-make-toc)
