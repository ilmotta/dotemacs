;;; -*- lexical-binding: t; -*-

(require 'lib-util)

(lib-util/pkg groovy-mode
  :ensure (:ref "c612ac1e9f742856914ad6e8eb9e9dc169f489ab")
  :defer t
  :mode (rx (or ".groovy" ".graddle") string-end))

(provide 'pkg-groovy-mode)
