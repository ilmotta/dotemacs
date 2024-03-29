;;; -*- lexical-binding: t; -*-

;;; Commentary:
;; Asynchronous source block execution for org-babel. `ob-async' depends on the
;; `emacs-async' package, which is a simple library for asynchronous processing
;; in Emacs.

;;; Code:

(require 'lib-util)

(defun pkg-ob-async/enable ()
  (require 'ob-async))

(lib-util/pkg ob-async
  :elpaca (:ref "9aac486073f5c356ada20e716571be33a350a982")
  :defer t
  :hook (org-mode-hook . pkg-ob-async/enable))

(provide 'pkg-ob-async)
