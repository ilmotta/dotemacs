;;; -*- lexical-binding: t; -*-

(require 'lib-util)

(lib-util/pkg general
  :elpaca t
  :demand t
  :config
  (general-create-definer my/general-mode-def
    :states '(normal visual insert emacs)
    :prefix my/local-leader
    :non-normal-prefix my/non-normal-prefix))

;; Block until current queue processed.
(elpaca-wait)

(provide 'pkg-general)
