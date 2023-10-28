;;; -*- lexical-binding: t; -*-
(require 'use-package)

;; Clojure re-frame navigation for fully qualified keywords.
(lib-util/pkg re-jump
  :straight (:host github :repo "oliyh/re-jump.el")
  :disabled t
  :defer t)

(provide 'pkg-re-jump)
