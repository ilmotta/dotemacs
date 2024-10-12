;;; -*- lexical-binding: t; -*-

;;; Commentary:
;; Increase the padding/spacing of Emacs frames and windows.

;;; Code:

(require 'lib-util)

(lib-util/pkg spacious-padding
  :elpaca (:host github
           :repo "protesilaos/spacious-padding"
           :ref "a3151f3c99d6b3b2d4644da88546476b3d31f0fe")
  :demand t
  :init
  (setq spacious-padding-subtle-mode-line t)

  (setq spacious-padding-widths
        '(:fringe-width 25
          :header-line-width 4
          :internal-border-width 6
          :mode-line-width 0
          :right-divider-width 1
          :scroll-bar-width 8
          :tab-line-width 6
          :tab-width 6))

  :config
  (spacious-padding-mode +1))

(provide 'pkg-spacious-padding)
