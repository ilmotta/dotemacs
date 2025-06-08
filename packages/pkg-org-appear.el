;;; -*- lexical-binding: t; -*-

;;; Commentary:
;; Toggle visibility of hidden Org mode element parts upon entering and leaving
;; an element.

;;; Code:
(require 'lib-util)

(lib-util/pkg org-appear
  :ensure (:host github
           :repo "awth13/org-appear"
           :ref "32ee50f8fdfa449bbc235617549c1bccb503cb09")
  :defer t
  :init
  (setq org-appear-delay 0)
  (setq org-appear-trigger 'always)
  (setq org-appear-elements '(bold italic underline strike-through verbatim code))
  (setq org-appear-autolinks nil)
  (setq org-appear-autoemphasis t)
  (setq org-appear-autokeywords t)
  (setq org-appear-autosubmarkers t))

(provide 'pkg-org-appear)
