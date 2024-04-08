;;; -*- lexical-binding: t; -*-

(require 'lib-util)

(lib-util/pkg ef-themes
  :elpaca (:host github
           :repo "protesilaos/ef-themes"
           :ref "96aaab126510bfd303558e2a9b85a745dab32f41")
  :defer t)

(provide 'pkg-ef-themes)
