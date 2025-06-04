;;; -*- lexical-binding: t; -*-

(require 'lib-util)

(lib-util/pkg sudo-edit
  :ensure (:ref "74eb1e6986461baed9a9269566ff838530b4379b")
  :defer t)

(provide 'pkg-sudo-edit)
