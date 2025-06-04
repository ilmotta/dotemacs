;;; -*- lexical-binding: t; -*-

;;; Commentary:
;; Allows Emacs to copy to and paste from the GUI clipboard when running in text
;; terminal.

;;; Code:

(require 'lib-util)

(lib-util/pkg xclip
  :ensure (:host github
           :repo "emacsmirror/xclip"
           :ref "7febe164de2a881b83b9d604d3c7cf20b69f422d"
           :branch nil)
  :if (and (not (display-graphic-p)) (not my/android?))
  :demand t
  :config
  (xclip-mode +1))

(provide 'pkg-xclip)
