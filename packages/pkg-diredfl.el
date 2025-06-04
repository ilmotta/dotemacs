;;; -*- lexical-binding: t; -*-

;;; Commentary:
;; Extra font lock rules for a more colorful dired.

;;; Code:

(require 'lib-util)

(lib-util/pkg diredfl
  :ensure (:ref "94bd99eeced6d52a5a7b9db3745239feafd633e2")
  :defer t
  :hook (dired-mode-hook . diredfl-mode))

(provide 'pkg-diredfl)
