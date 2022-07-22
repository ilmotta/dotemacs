;;; -*- lexical-binding: t; -*-

;;; Commentary:
;; Convert buffer text and decorations to HTML. This package is required by
;; `org-html-export-to-html'.

;;; Code:

(my/package htmlize
  :straight t
  :defer t)

(provide 'pkg-htmlize)
