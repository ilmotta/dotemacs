;;; -*- lexical-binding: t; -*-

(require 'lib-util)

(lib-util/pkg terraform-mode
  :ensure (:ref "e67459fefc871fdbf20e27be8f85b98b10b97b1b")
  :defer t)

(provide 'pkg-terraform-mode)
