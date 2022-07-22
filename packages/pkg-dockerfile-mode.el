;;; -*- lexical-binding: t; -*-

(my/package dockerfile-mode
  :straight t
  :defer t
  ;; Any file starting with "Dockerfile" should enable this mode.
  :mode (("^Dockerfile" . dockerfile-mode)))

(provide 'pkg-dockerfile-mode)
