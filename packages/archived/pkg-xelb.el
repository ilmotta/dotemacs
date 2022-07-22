;;; -*- lexical-binding: t; -*-
(require 'use-package)

;; Because we use the repos ch11ng/exwm and ch11ng/xelb (an EXWM dependency)
;; to install EXWM, we need to make sure =xelb= is installed first, otherwise
;; straight generates a warning about having multiple recipes for xelb. I've
;; tried to set ':after xelb' in the exwm use-package configuration, but it
;; didn't work at all.
;;
;; XELB (X protocol Emacs Lisp Binding) is a pure Elisp implementation of X11
;; protocol based on the XML description files from XCB project. This is a
;; dependency of the exwm package. Here the ch11ng/xelb repository replaces the
;; default straight recipe.
(my/package xelb
  :disabled t
  :straight (:type git :host github :repo "ch11ng/xelb")
  :demand t)

(provide 'pkg-xelb)
