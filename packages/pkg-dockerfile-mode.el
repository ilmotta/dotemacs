;;; -*- lexical-binding: t; -*-

(require 'lib-util)

(lib-util/pkg dockerfile-mode
  :ensure (:ref "52c6c00da1d31c0b6c29c74335b3af63ed6bf06c")
  :defer t
  ;; Any file starting with "Dockerfile" should enable this mode.
  :mode (("^Dockerfile" . dockerfile-mode)))

(provide 'pkg-dockerfile-mode)
