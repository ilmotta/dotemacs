;;; -*- lexical-binding: t; -*-

(my/package dumb-jump
  :straight t
  :defer t
  :init
  ;; When set to rg it will still use git-grep if it's a git project (because
  ;; it's the fastest), but will you use whatever you set here in any
  ;; other situation.
  (setq dumb-jump-prefer-searcher 'rg))

(provide 'pkg-dumb-jump)
