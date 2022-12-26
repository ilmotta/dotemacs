;;; -*- lexical-binding: t; -*-

(my/package
  (hide-mode-line :ref "bc5d293576c5e08c29e694078b96a5ed85631942")
  :defer t
  :hook ((pdf-annot-list-mode-hook
          flycheck-error-list-mode-hook) . hide-mode-line-mode))

(provide 'pkg-hide-mode-line)
