;;; -*- lexical-binding: t; -*-
(require 'use-package)

;; Clojure re-frame navigation for fully qualified keywords.
(my/package re-jump
  :straight (:host github :repo "oliyh/re-jump.el")
  :disabled t
  :defer t)

(provide 'pkg-re-jump)
