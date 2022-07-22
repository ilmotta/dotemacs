;;; -*- lexical-binding: t; -*-
;;; Code:

(defvar pkg-org-roam/tasks-file
  "~/data/repos/notes/20200827220222.org")

;;;###autoload
(defun pkg-org-roam/pop-to-buffer-tasks ()
  (interactive)
  (pop-to-buffer (find-file-noselect (file-truename pkg-org-roam/tasks-file))))

(my/package emacsql
  :straight (:host github :repo "skeeto/emacsql")
  :defer t)

(my/package emacsql-sqlite
  :straight (:host github :repo "skeeto/emacsql")
  :defer t)

;; Org-roam is a solution for effortless non-hierarchical note-taking with
;; Org-mode.
(my/package org-roam
  :straight t
  :defer t

  :init
  (general-def
    :keymaps 'org-mode-map
    :prefix "C-c n"
    "i" #'org-roam-node-insert)

  ;; <leader> n --- node
  (general-def
    :keymaps 'my/keys-mode-map
    :states '(normal insert emacs visual)
    :prefix (concat my/leader " n")
    :non-normal-prefix "C-c n"
    "t a" #'org-roam-tag-add
    "t d" #'org-roam-tag-remove
    "T" #'pkg-org-roam/pop-to-buffer-tasks
    "f" #'org-roam-node-find
    "g" #'org-roam-graph
    "r" #'org-roam-refile
    "R" #'org-roam-db-sync)

  (setq org-roam-directory (expand-file-name "~/data/repos/notes/"))
  (setq org-roam-db-location (concat my/cache-dir "org-roam.db"))
  (setq org-roam-list-files-commands '(rg fd find))
  (setq org-roam-db-gc-threshold most-positive-fixnum)
  (setq org-roam-verbose nil)
  (setq org-roam-link-auto-replace nil)
  (setq org-roam-database-connector 'sqlite)

  ;; There's a noticeable lag after saving.
  (setq org-roam-db-update-on-save nil)

  (setq org-roam-node-display-template (concat "${title:48} "
                                               (propertize "${pkg-org-roam/file-title:48}" 'face 'org-roam-title)
                                               (propertize "${tags:15}" 'face 'org-tag)))

  :config
  (cl-defmethod org-roam-node-pkg-org-roam/file-title ((node org-roam-node))
    "Returns the node file title, unless it's the same as the node
title, in which case it returns an empty string."
    (let ((file-title (org-roam-node-file-title node))
          (title (org-roam-node-title node)))
      (if (equal file-title title)
          ""
        file-title)))

  (lib-util/add-hook-once 'org-mode-hook #'org-roam-db-autosync-enable))

(provide 'pkg-org-roam)
