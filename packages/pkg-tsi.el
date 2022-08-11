;;; -*- lexical-binding: t; -*-

;;; Commentary:
;; Use the syntax tree provided by the tree-sitter minor mode as the basis for
;; indentation.

;;; Code:
(my/package tsi
  :unless my/android?
  :straight (:host github :repo "orzechowskid/tsi.el")
  :defer t
  :hook ((javascript-mode-hook typescript-mode-hook) . tsi-typescript-mode))

(provide 'pkg-tsi)
