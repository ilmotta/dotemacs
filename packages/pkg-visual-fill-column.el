;;; -*- lexical-binding: t; -*-

;;; Commentary:
;; Minor mode that mimics the effect of fill-column in visual-line-mode. Instead
;; of wrapping lines at the window edge, which is the standard behavior of
;; visual-line-mode, it wraps lines at fill-column.

;;; Code:

(my/package visual-fill-column
  :straight t
  :defer t)

(provide 'pkg-visual-fill-column)
