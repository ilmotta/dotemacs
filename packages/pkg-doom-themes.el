;;; -*- lexical-binding: t; -*-

(require 'lib-util)

(lib-util/pkg doom-themes
  :elpaca (:ref "3b2422b208d28e8734b300cd3cc6a7f4af5eba55")
  :disabled t ; Doesn't work with latest Emacs from 2024-04-08
  :defer t)

(provide 'pkg-doom-themes)
