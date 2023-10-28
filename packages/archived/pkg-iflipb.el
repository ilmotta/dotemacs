;;; -*- lexical-binding: t; -*-
(require 'use-package)

;; Flip between recently visited buffers in a way that resembles what
;; Alt-(Shift-)TAB does in other graphical window managers.
(lib-util/pkg iflipb
  :straight t
  :defer t)

(provide 'pkg-iflipb)
