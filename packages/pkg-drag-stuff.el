;;; -*- lexical-binding: t; -*-

(my/package drag-stuff
  :straight t
  :defer t
  :init
  (general-def
    :keymaps 'my/keys-mode-map
    "M-S-<up>" #'drag-stuff-up
    "M-S-<down>" #'drag-stuff-down))

(provide 'pkg-drag-stuff)
