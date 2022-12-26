;;; -*- lexical-binding: t; -*-

(my/package
  (yaml-mode :ref "3fcb36d6039bef57e2a0f6e24c51f623c0bf5fb7")
  :defer t
  :mode (rx "." (or "yml" "yaml") string-end))

(provide 'pkg-yaml-mode)
