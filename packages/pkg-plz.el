;;; -*- lexical-binding: t; -*-

;;; Commentary:
;; plz is an HTTP library for Emacs. It uses curl as a backend, which avoids
;; some of the issues with using Emacs’s built-in url library.

;;; Code:

(my/package
  (plz :ref "b6072edeec1f0e2465d273db74a8f2f7726e6bce"
       :fetcher github
       :repo "alphapapa/plz.el"
       :files (:defaults))
  :defer t)

(provide 'pkg-plz)
