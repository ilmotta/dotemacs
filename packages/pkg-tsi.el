;;; -*- lexical-binding: t; -*-

;;; Commentary:
;; Use the syntax tree provided by the tree-sitter minor mode as the basis for
;; indentation.

;;; Code:

(require 'lib-util)

(lib-util/pkg tsi
  :elpaca (:host github :repo "orzechowskid/tsi.el")
  :unless my/android?
  :defer t
  :hook ((javascript-mode-hook typescript-mode-hook) . tsi-typescript-mode))

(provide 'pkg-tsi)
