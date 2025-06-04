;;; -*- lexical-binding: t; -*-

;;; Commentary:
;; Small utility functions that allow for fish-style trunctated directories in
;; eshell and for example modeline.

;;; Code:

(require 'lib-util)

(lib-util/pkg shrink-path
  :ensure (:ref "c14882c8599aec79a6e8ef2d06454254bb3e1e41")
  :defer t)

(provide 'pkg-shrink-path)
