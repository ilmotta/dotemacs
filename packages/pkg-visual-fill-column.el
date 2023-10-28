;;; -*- lexical-binding: t; -*-

;;; Commentary:
;; Minor mode that mimics the effect of fill-column in visual-line-mode. Instead
;; of wrapping lines at the window edge, which is the standard behavior of
;; visual-line-mode, it wraps lines at fill-column.

;;; Code:

(lib-util/pkg visual-fill-column
  :elpaca (:ref "453d698d7fc243a547665f8ba43c55eee574e0db")
  :defer t)

(provide 'pkg-visual-fill-column)
