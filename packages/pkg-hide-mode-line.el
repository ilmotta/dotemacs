;;; -*- lexical-binding: t; -*-

(my/package hide-mode-line
  :straight t
  :defer t
  :hook ((pdf-annot-list-mode-hook
          flycheck-error-list-mode-hook) . hide-mode-line-mode))

(provide 'pkg-hide-mode-line)
