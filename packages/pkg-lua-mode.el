;;; -*- lexical-binding: t; -*-

(require 'lib-util)

(lib-util/pkg lua-mode
  :elpaca (:ref "ad639c62e38a110d8d822c4f914af3e20b40ccc4")
  :defer t)

(provide 'pkg-lua-mode)
