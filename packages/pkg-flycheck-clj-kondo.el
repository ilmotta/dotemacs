;;; -*- lexical-binding: t; -*-

(require 'lib-util)

(lib-util/pkg flycheck-clj-kondo
  :elpaca (:ref "e38c67ba9db1ea1cbe1b61ab39b506c05efdcdbf")
  :defer t)

(provide 'pkg-flycheck-clj-kondo)
