;;; -*- lexical-binding: t; -*-

(my/package tree-sitter
  :unless my/android?
  :straight t
  :defer t
  :hook ((css-mode-hook
          java-mode-hook
          js-mode-hook
          json-mode-hook
          python-mode-hook
          rjsx-mode-hook
          ruby-mode-hook
          sh-mode-hook
          typescript-mode-hook) . tree-sitter-hl-mode)

  :init
  (define-derived-mode typescript-tsx-mode typescript-mode
    "TypeScript TSX")

  (add-to-list 'auto-mode-alist '("\\.tsx?\\'" . typescript-tsx-mode))

  :config
  (add-to-list 'tree-sitter-major-mode-language-alist '(typescript-tsx-mode . tsx)))

(provide 'pkg-tree-sitter)
