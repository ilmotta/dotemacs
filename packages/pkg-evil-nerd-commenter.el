;;; -*- lexical-binding: t; -*-

;;; Commentary:
;; Comment/uncomment lines efficiently. Like Nerd Commenter in Vim.

;;; Code:

(require 'lib-util)

(lib-util/pkg evil-nerd-commenter
  :ensure (:ref "8c0f23d46a3927b9f83c1c2c4590be53d0b740db")
  :defer t)

(provide 'pkg-evil-nerd-commenter)
