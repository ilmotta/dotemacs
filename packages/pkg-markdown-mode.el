;;; -*- lexical-binding: t; -*-

(require 'lib-util)

(lib-util/pkg markdown-mode
  :elpaca (:ref "d95107f5b77d6c010e89259e05adfcd79a21f26a")
  :defer t
  :mode (("\\.md\\'" . markdown-mode)
         ("\\.markdown\\'" . markdown-mode)))

(provide 'pkg-markdown-mode)
