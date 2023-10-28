;;; -*- lexical-binding: t; -*-

;;; Commentary:
;; Allows you to edit a grep buffer and apply those changes to the file buffer
;; like sed interactively.

;;; Code:
(lib-util/pkg wgrep
  :elpaca (:ref "f9687c28bbc2e84f87a479b6ce04407bb97cfb23")
  :defer t
  :init
  (when my/evil-p
    (general-def
      :keymaps 'grep-mode-map
      :states 'normal
      "i" #'wgrep-change-to-wgrep-mode)))

(provide 'pkg-wgrep)
