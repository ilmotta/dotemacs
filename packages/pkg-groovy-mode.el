;;; -*- lexical-binding: t; -*-

(my/package groovy-mode
  :straight t
  :defer t
  :mode (rx (or ".groovy" ".graddle") string-end))

(provide 'pkg-groovy-mode)
