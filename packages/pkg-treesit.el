;;; -*- lexical-binding: t; -*-

(require 'lib-util)

(lib-util/pkg treesit
  :ensure nil
  :init
  ;; Maximum amount of syntax highlighting. Level 4 can be too colorful in some
  ;; major modes.
  (setq-default treesit-font-lock-level 3)

  ;; Some .so files can be downloaded/built from
  ;; https://github.com/casouri/tree-sitter-module/releases.
  (setq treesit-extra-load-path
        (list (file-name-concat my/local-dir "tree-sitter-grammars"))))

(provide 'pkg-treesit)
