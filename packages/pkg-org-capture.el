;;; -*- lexical-binding: t; -*-
;;; Templates

(defun my/org-capture--task-templates ()
  `(("t" ,(concat (all-the-icons-faicon "tasks") " Task")
     entry (file "~/data/repos/notes/20200827220222.org")
     "* TODO %^{Description}\n:PROPERTIES:\n:ID: %(org-id-new)\n:CREATED: %T\n:END:"
     :immediate-finish t)))

(defun my/note-id ()
  (format-time-string my/note-id-time-string (current-time) "UTC0"))

(defun my/org-capture--note-templates ()
  `(("n" ,(concat (all-the-icons-faicon "sticky-note" :v-adjust -0.1) " Note")
     plain
     (file (lambda () (format "~/data/repos/notes/%s.org" (my/note-id))))
     ,(concat
       ":PROPERTIES:\n"
       ":ID: " (lib-util/uuid) "\n"
       ":END:\n"
       "#+SETUPFILE: setup.org\n#+TITLE: Note%?\n")
     :immediate-finish t
     :jump-to-captured t
     :no-save t)))

;;; Package

;; Display battery status in the mode line.
(my/package org-capture
  :straight (:type built-in)
  :defer t

  :init
  (general-def
    :keymaps 'my/keys-mode-map
    "C-c c" #'org-capture)

  :config
  (with-eval-after-load 'all-the-icons
    (setq org-capture-templates
          (append (my/org-capture--task-templates)
                  (my/org-capture--note-templates)))))

(provide 'pkg-org-capture)
