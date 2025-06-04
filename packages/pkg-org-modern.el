;;; -*- lexical-binding: t; -*-

;;; Code:

(require 'lib-util)

(lib-util/pkg org-modern
  :ensure (:host github
           :repo "minad/org-modern"
           :ref "33f694c57113418c7e739a824ff57dc9e7eb413c")
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
  (setq org-modern-star 'replace)
  (setq org-modern-replace-stars "◉⊙◈◇✳")
  (setq org-modern-statistics t)
  (setq org-modern-table nil)
  (setq org-modern-table-horizontal 1)
  (setq org-modern-table-vertical 1)
  (setq org-modern-tag t)
  (setq org-modern-timestamp t)
  (setq org-modern-todo t)

  :config
  (with-eval-after-load 'org
    (setq org-modern-todo-faces org-todo-keyword-faces)))

(provide 'pkg-org-modern)
