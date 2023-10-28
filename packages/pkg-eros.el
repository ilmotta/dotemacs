;;; -*- lexical-binding: t; -*-

;;; Commentary:
;; Show elisp evaluation results in an overlay.

;;; Code:

(require 'lib-util)

(lib-util/pkg eros
  :elpaca (:ref "dd8910279226259e100dab798b073a52f9b4233a")
  :defer t
  :hook ((clojure-mode-hook
          emacs-lisp-mode-hook
          scheme-mode-hook) . eros-mode))

(provide 'pkg-eros)
