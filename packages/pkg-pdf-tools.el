;;; -*- lexical-binding: t; -*-

;;; Commentary:
;; A DocView replacement for PDF files.

;;; Code:

(my/package pdf-tools
  :when (equal 'gnu/linux my/system-type)
  :defer t
  :mode ("\\.pdf\\'" . pdf-view-mode)
  :init
  (setq pdf-view-display-size 'fit-page
        pdf-view-use-scaling t ; Images can be scaled down for rendering
        pdf-view-continuous t))

(provide 'pkg-pdf-tools)
