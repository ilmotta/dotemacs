;;; -*- lexical-binding: t; -*-

(require 'lib-util)

(lib-util/pkg plantuml-mode
  :ensure (:ref "ea45a13707abd2a70df183f1aec6447197fc9ccc")
  :defer t
  :mode ((rx ".puml" string-end) . plantuml-mode)
  :init
  (setq plantuml-default-exec-mode 'executable))

(provide 'pkg-plantuml-mode)
