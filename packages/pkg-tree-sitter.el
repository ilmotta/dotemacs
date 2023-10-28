;;; -*- lexical-binding: t; -*-

(lib-util/pkg tree-sitter
  :unless my/android?
  :defer t
  :hook ((java-mode-hook
          js-mode-hook
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
