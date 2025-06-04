;;; -*- lexical-binding: t; -*-

(require 'lib-util)

(lib-util/pkg kotlin-mode
  :ensure (:ref "55eed95033a59d7448a4b2bc11879e62c05e361b")
  :defer t)

(provide 'pkg-kotlin-mode)
