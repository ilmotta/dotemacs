;;; -*- lexical-binding: t; -*-

;;; Code:

(require 'lib-util)

(lib-util/pkg org-modern
  :elpaca (:host github
           :repo "minad/org-modern"
           :ref "d812a192f040a9e7785a53f144de1800d52b9f0d")
  :defer t
  :hook (org-mode-hook . org-modern-mode)
  :init
  (setq org-modern-block-name t)
  (setq org-modern-checkbox '((88 . "☑") (32 . "☐")))
  (setq org-modern-hide-stars 'leading)
  (setq org-modern-horizontal-rule t)
  (setq org-modern-keyword t)
  (setq org-modern-label-border 'auto)
  (setq org-modern-priority t)
  (setq org-modern-progress ["○" "◔" "◐" "◕" "●"])
  (setq org-modern-star ["⊚" "✤" "⊡" "◈"])
  (setq org-modern-statistics t)
  (setq org-modern-table nil)
  (setq org-modern-table-horizontal 1)
  (setq org-modern-table-vertical 1)
  (setq org-modern-tag t)
  (setq org-modern-timestamp t)
  (setq org-modern-todo t))

(provide 'pkg-org-modern)
