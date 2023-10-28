;;; -*- lexical-binding: t; -*-

(lib-util/pkg hide-mode-line
  :elpaca (:ref "bc5d293576c5e08c29e694078b96a5ed85631942")
  :defer t
  :hook ((pdf-annot-list-mode-hook
          flycheck-error-list-mode-hook) . hide-mode-line-mode))

(provide 'pkg-hide-mode-line)
