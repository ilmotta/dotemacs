;;; -*- lexical-binding: t; -*-

(my/package cargo
  :elpaca (:ref "d2720c8dc7ac3b18ce112a886d3b8696797d01cb")
  :defer t
  :hook (rust-mode-hook . cargo-minor-mode))

(provide 'pkg-cargo)
