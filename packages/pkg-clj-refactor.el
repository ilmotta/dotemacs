;;; -*- lexical-binding: t; -*-

(require 'lib-util)

(lib-util/pkg clj-refactor
  :ensure (:ref "dc1bbc8cdaa723bdbb6669ea7d280625c370755d")
  :defer t
  :hook (clojure-mode-hook . clj-refactor-mode)
  :init
  ;; The implementation of magic requires blocks Emacs after typing "/", which
  ;; is very annoying.
  (setq cljr-magic-requires nil))

(provide 'pkg-clj-refactor)
