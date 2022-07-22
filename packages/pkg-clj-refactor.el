;;; -*- lexical-binding: t; -*-

(my/package clj-refactor
  :straight t
  :defer t
  :hook (clojure-mode-hook . clj-refactor-mode))

(provide 'pkg-clj-refactor)
