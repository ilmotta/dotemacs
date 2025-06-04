;;; -*- lexical-binding: t; -*-

(require 'lib-util)

(lib-util/pkg php-mode
  :ensure (:ref "d01cfc9cd51706e076bf7e5cbf0cfa7ee885efb4")
  :defer t)

(provide 'pkg-php-mode)
