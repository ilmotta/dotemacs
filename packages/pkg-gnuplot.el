;;; -*- lexical-binding: t; -*-

(require 'lib-util)

(lib-util/pkg gnuplot
  :ensure (:ref "fe7ce76d797b34214178ac8e470f2fa9a63b2520")
  :defer t)

(provide 'pkg-gnuplot)
