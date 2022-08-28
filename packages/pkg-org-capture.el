;;; -*- lexical-binding: t; -*-

;;; Utilities

(defun pkg-org-capture/note-id ()
  (format-time-string my/note-id-time-string (current-time) "UTC0"))

;;; Templates

(defun pkg-org-capture/note-new-path ()
  (file-truename (format "~/data/repos/notes/%s.org" (pkg-org-capture/note-id))))

(defvar pkg-org-capture/template-note
  (string-join (list ":properties:"
                     ":id: %(lib-util/uuid)"
                     ":end:"
                     "#+setupfile: setup.org"
                     "#+title: Note%?\n")
               "\n"))

(defvar pkg-org-capture/template-todo
  (string-join (list "* TODO %^{Description}"
                     ":properties:"
                     ":id: %(lib-util/uuid)"
                     ":created: %T"
                     ":end:")
               "\n"))

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
          (append
           `(("n" ,(concat (all-the-icons-faicon "sticky-note" :v-adjust -0.1) " Note")
              plain
              (file pkg-org-capture/note-new-path)
              ,pkg-org-capture/template-note
              :immediate-finish t
              :jump-to-captured t
              :no-save t))
           `(("t" ,(concat (all-the-icons-faicon "tasks") " Task")
              entry (file "~/data/repos/notes/20200827220222.org")
              ,pkg-org-capture/template-todo
              :immediate-finish t))))))

(provide 'pkg-org-capture)
