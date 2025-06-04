;;; -*- lexical-binding: t; -*-

(require 'lib-util)

(lib-util/pkg nix-mode
  :ensure (:ref "54e5626829168e22126b233e079f04dff3c71b90")
  :defer t)

(provide 'pkg-nix-mode)
