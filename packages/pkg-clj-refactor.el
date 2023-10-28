;;; -*- lexical-binding: t; -*-

(lib-util/pkg clj-refactor
  :elpaca (:ref "8300d5cab861668f313fbbbb3e2926e3e5130e86")
  :defer t
  :hook (clojure-mode-hook . clj-refactor-mode)
  :init
  ;; The implementation of magic requires blocks Emacs after typing "/", which
  ;; is very annoying.
  (setq cljr-magic-requires nil))

(provide 'pkg-clj-refactor)
