;;; -*- lexical-binding: t; -*-

(require 'lib-util)

(lib-util/pkg bookmark
  :elpaca nil
  :init
  ;; File in which to save bookmarks by default.
  (setq bookmark-default-file (file-name-concat my/cache-dir "bookmarks"))

  (setq bookmark-set-fringe-mark t))

(provide 'pkg-bookmark)
