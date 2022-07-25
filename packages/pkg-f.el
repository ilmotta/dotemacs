;;; -*- lexical-binding: t; -*-

(my/package f
  :straight (:files ("f.el" "f-shortdoc.el") :host github :repo "rejeep/f.el")
  :defer t)

(provide 'pkg-f)
