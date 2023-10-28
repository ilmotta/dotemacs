;;; -*- lexical-binding: t; -*-

;;; Commentary:
;; A DocView replacement for PDF files.

;;; Code:

(require 'lib-util)

(lib-util/pkg pdf-tools
  :elpaca (:ref "b8079e4ebc2936f9772657332d50936350a65825")
  :when (equal 'gnu/linux my/system-type)
  :defer t
  :mode ("\\.pdf\\'" . pdf-view-mode)
  :init
  (setq pdf-view-display-size 'fit-page
        pdf-view-use-scaling t ; Images can be scaled down for rendering
        pdf-view-continuous t))

(provide 'pkg-pdf-tools)
