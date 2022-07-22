;;; -*- lexical-binding: t; -*-

(my/package yaml-mode
  :straight t
  :defer t
  :mode (rx "." (or "yml" "yaml") string-end))

(provide 'pkg-yaml-mode)
