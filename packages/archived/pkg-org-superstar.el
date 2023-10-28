;;; -*- lexical-binding: t; -*-
(require 'use-package)

;; Prettify headings and plain lists in Org mode. This package is a direct
;; descendant of org-bullets.
(lib-util/pkg org-superstar
  :straight t
  :defer t
  :init
  (setq org-superstar-headline-bullets-list
        '(
          ?⊚
          ?✤
          ?⊡
          ?჻
          ;; ?⌬
          ;; ?⏣
          ;; ?⋑
          ;; ?๑
          ))

  ;; `org-backward-heading-same-level' doesn't work if the leading stars are not
  ;; present.
  (setq org-superstar-remove-leading-stars nil)

  (setq org-superstar-leading-bullet " ․"))

(provide 'pkg-org-superstar)
