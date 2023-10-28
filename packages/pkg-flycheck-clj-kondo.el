;;; -*- lexical-binding: t; -*-

(require 'lib-util)

(lib-util/pkg flycheck-clj-kondo
  :elpaca (:ref "ff7bed2315755cfe02ef471edf522e27b78cd5ca")
  :defer t)

(provide 'pkg-flycheck-clj-kondo)
