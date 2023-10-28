;;; -*- lexical-binding: t; -*-

;;; Commentary:
;; Ported Promises/A+ features to Emacs Lisp.

;;; Code:

(require 'lib-util)

(lib-util/pkg promise
  :elpaca (:ref "cec51feb5f957e8febe6325335cf57dc2db6be30")
  :defer t)

(provide 'pkg-promise)
