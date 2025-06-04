;;; -*- lexical-binding: t; -*-

(require 'lib-util)

(lib-util/pkg drag-stuff
  :ensure (:ref "6d06d846cd37c052d79acd0f372c13006aa7e7c8")
  :defer t
  :init
  (general-def
    :keymaps 'my/keys-mode-map
    "M-S-<up>" #'drag-stuff-up
    "M-S-<down>" #'drag-stuff-down))

(provide 'pkg-drag-stuff)
