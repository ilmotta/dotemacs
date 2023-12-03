;;; -*- lexical-binding: t; -*-

(require 'lib-util)

(lib-util/pkg tree-sitter-langs
  :disabled t
  :unless my/android?
  :defer t)

(provide 'pkg-tree-sitter-langs)
