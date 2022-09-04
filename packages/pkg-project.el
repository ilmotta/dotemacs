;;; -*- lexical-binding: t; -*-

(defun pkg-project/locate-project (dir)
  (let ((override (locate-dominating-file dir ".project.el")))
    (if override
        (cons 'vc override)
      nil)))

(my/package project
  :straight (:type built-in)
  :demand t
  :hook (project-find-functions . project-try-vc)
  :hook (project-find-functions . pkg-project/locate-project)

  :init
  (general-def
    :keymaps 'override
    :prefix my/leader
    :states '(normal visual)
    "p" '(:keymap project-prefix-map :package project))

  (general-def
    :keymaps 'project-prefix-map
    ;; Leave "s" for the Search mnemonic, as I use other commands to open
    ;; shells.
    "s" nil
    "." #'lib-util/project-switch-to-dotfiles)

  (setq project-list-file (concat my/cache-dir "projects"))
  (setq project-switch-commands #'project-dired))

(provide 'pkg-project)
