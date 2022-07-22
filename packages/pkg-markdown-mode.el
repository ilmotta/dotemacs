;;; -*- lexical-binding: t; -*-

(my/package markdown-mode
  :straight t
  :defer t
  :mode (("\\.md\\'" . markdown-mode)
         ("\\.markdown\\'" . markdown-mode)))

(provide 'pkg-markdown-mode)
