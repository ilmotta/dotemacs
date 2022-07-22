;;; -*- lexical-binding: t; -*-

(my/package general
  :straight t
  :demand t
  :config
  (general-create-definer my/general-mode-def
    :states '(normal visual insert emacs)
    :prefix my/local-leader
    :non-normal-prefix my/non-normal-prefix))

(provide 'pkg-general)
