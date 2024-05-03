;;; -*- lexical-binding: t; -*-

;;; Commentary:
;; Increase the padding/spacing of Emacs frames and windows.

;;; Code:

(require 'lib-util)

(lib-util/pkg spacious-padding
  :elpaca (:host github
           :repo "protesilaos/spacious-padding"
           :ref "e48f3335f50217e40081631abacc40964150f3ab")
  :defer t
  :init
  (setq spacious-padding-subtle-mode-line t)

  (setq spacious-padding-widths
        '(:fringe-width 25
          :header-line-width 4
          :internal-border-width 6
          :mode-line-width 6
          :right-divider-width 45
          :scroll-bar-width 8
          :tab-line-width 6
          :tab-width 6)))

(provide 'pkg-spacious-padding)
