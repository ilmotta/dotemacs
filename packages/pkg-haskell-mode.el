;;; -*- lexical-binding: t; -*-

(require 'lib-util)

(lib-util/pkg haskell-mode
  :elpaca (:ref "79eaf444a72109f93f552abb53f834cc63bbf9f2")
  :defer t)

(provide 'pkg-haskell-mode)
