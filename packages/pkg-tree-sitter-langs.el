;;; -*- lexical-binding: t; -*-

(lib-util/pkg tree-sitter-langs
  :unless my/android?
  :defer t)

(provide 'pkg-tree-sitter-langs)
