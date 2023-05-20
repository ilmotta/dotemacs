;;; -*- lexical-binding: t; -*-

(my/package general
  :elpaca (:ref "9651024e7f40a8ac5c3f31f8675d3ebe2b667344")
  :demand t
  :config
  (general-create-definer my/general-mode-def
    :states '(normal visual insert emacs)
    :prefix my/local-leader
    :non-normal-prefix my/non-normal-prefix))

;; Block until current queue processed.
(elpaca-wait)

(provide 'pkg-general)
