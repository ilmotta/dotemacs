;;; -*- lexical-binding: t; -*-
;;; Code:

(my/package
  (org-modern :host github
              :repo "minad/org-modern"
              :ref "010eade723881ca234a12bd94b791e2000cd2a15")
  :defer t
  :hook (org-mode-hook . org-modern-mode)
  :init
  (setq org-modern-block t)
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
  (setq org-modern-todo t)
  (setq org-modern-variable-pitch nil))

(provide 'pkg-org-modern)
