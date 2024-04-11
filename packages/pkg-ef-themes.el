;;; -*- lexical-binding: t; -*-

(require 'lib-util)

(lib-util/pkg ef-themes
  :elpaca (:host github
           :repo "protesilaos/ef-themes"
           :ref "96aaab126510bfd303558e2a9b85a745dab32f41")
  :defer t
  :init
  ;; Use `ef-themes-preview-colors-current' to see all available color names.
  (setq ef-maris-dark-palette-overrides
        ;; The default bg-main color is too dark.
        '((bg-main bg-dim))))

(provide 'pkg-ef-themes)
