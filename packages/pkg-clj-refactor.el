;;; -*- lexical-binding: t; -*-

(my/package clj-refactor
  :straight t
  :defer t
  :hook (clojure-mode-hook . clj-refactor-mode)
  :init
  ;; The implementation of magic requires blocks Emacs after typing "/", which
  ;; is very annoying.
  (setq cljr-magic-requires nil))

(provide 'pkg-clj-refactor)
