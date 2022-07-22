;;; -*- lexical-binding: t; -*-

(my/package plantuml-mode
  :straight t
  :defer t
  :mode ((rx ".puml" string-end) . plantuml-mode)
  :init
  (setq plantuml-jar-path (expand-file-name "~/.local/bin/plantuml.jar")))

(provide 'pkg-plantuml-mode)
