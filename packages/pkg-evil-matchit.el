;;; -*- lexical-binding: t; -*-

;;; Commentary:
;; Press "%" to jump between matched tags ("<div>" and "</div>" in html, etc).

;;; Code:

(my/package evil-matchit
  :straight t
  :defer t
  :hook ((typescript-mode-hook
          typescript-tsx-mode-hook
          js-mode-hook) . evil-matchit-mode))

(provide 'pkg-evil-matchit)
