;;; -*- lexical-binding: t; -*-

;;; Commentary:
;; Kmonad major mode type.

;;; Code:

(my/package kbd-mode
  :elpaca (:host github
           :repo "kmonad/kbd-mode"
           :ref "96178a43d3c9ea3167362513fe4c3fdeb7074e9f")
  :defer t)

(provide 'pkg-kbd-mode)
