;;; -*- lexical-binding: t; -*-

;;; Commentary:
;; Press "%" to jump between matched tags ("<div>" and "</div>" in html, etc).

;;; Code:

(require 'lib-util)

(lib-util/pkg evil-matchit
  :elpaca (:ref "ec3dd819983b2d824142efddd46ef29b46a7c454")
  :defer t
  :hook ((typescript-mode-hook
          typescript-tsx-mode-hook
          js-mode-hook) . evil-matchit-mode))

(provide 'pkg-evil-matchit)
