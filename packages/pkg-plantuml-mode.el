;;; -*- lexical-binding: t; -*-

(my/package
  (plantuml-mode :ref "ea45a13707abd2a70df183f1aec6447197fc9ccc")
  :defer t
  :mode ((rx ".puml" string-end) . plantuml-mode)
  :init
  (setq plantuml-jar-path (expand-file-name "~/.local/bin/plantuml.jar")))

(provide 'pkg-plantuml-mode)
