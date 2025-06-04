;;; -*- lexical-binding: t; -*-

;;; Commentary:
;; Major modes for various Git configuration files (e.g. .gitconfig).

;;; Code:

(require 'lib-util)

(lib-util/pkg git-modes
  :ensure (:ref "be96ef14fab6a2d76cca3ebf9a15b462a695923d")
  :defer t)

(provide 'pkg-git-modes)
