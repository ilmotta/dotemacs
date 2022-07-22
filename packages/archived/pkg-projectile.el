;;; -*- lexical-binding: t; -*-
(require 'use-package)

(defvar my/projectile-command-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd ".")     #'lib-util/projectile-switch-to-dotfiles)
    (define-key map (kbd "4 a")     #'projectile-find-other-file-other-window)
    (define-key map (kbd "4 b")     #'projectile-switch-to-buffer-other-window)
    (define-key map (kbd "4 C-o")   #'projectile-display-buffer)
    (define-key map (kbd "4 d")     #'projectile-find-dir-other-window)
    (define-key map (kbd "4 D")     #'projectile-dired-other-window)
    (define-key map (kbd "4 f")     #'projectile-find-file-other-window)
    (define-key map (kbd "4 g")     #'projectile-find-file-dwim-other-window)
    (define-key map (kbd "4 t")     #'projectile-find-implementation-or-test-other-window)
    (define-key map (kbd "5 a")     #'projectile-find-other-file-other-frame)
    (define-key map (kbd "5 b")     #'projectile-switch-to-buffer-other-frame)
    (define-key map (kbd "5 d")     #'projectile-find-dir-other-frame)
    (define-key map (kbd "5 D")     #'projectile-dired-other-frame)
    (define-key map (kbd "5 f")     #'projectile-find-file-other-frame)
    (define-key map (kbd "5 g")     #'projectile-find-file-dwim-other-frame)
    (define-key map (kbd "5 t")     #'projectile-find-implementation-or-test-other-frame)
    (define-key map (kbd "!")       #'projectile-run-shell-command-in-root)
    (define-key map (kbd "&")       #'projectile-run-async-shell-command-in-root)
    (define-key map (kbd "a")       #'projectile-find-other-file)
    (define-key map (kbd "b")       #'projectile-switch-to-buffer)
    (define-key map (kbd "d")       #'projectile-find-dir)
    (define-key map (kbd "D")       #'projectile-dired)
    (define-key map (kbd "e")       #'projectile-recentf)
    (define-key map (kbd "E")       #'projectile-edit-dir-locals)
    (define-key map (kbd "f")       #'projectile-find-file)
    (define-key map (kbd "g")       #'projectile-find-file-dwim)
    (define-key map (kbd "F")       #'projectile-find-file-in-known-projects)
    (define-key map (kbd "i")       #'projectile-invalidate-cache)
    (define-key map (kbd "I")       #'projectile-ibuffer)
    (define-key map (kbd "j")       #'projectile-find-tag)
    (define-key map (kbd "k")       #'projectile-kill-buffers)
    (define-key map (kbd "l")       #'projectile-find-file-in-directory)
    (define-key map (kbd "m")       #'projectile-commander)
    (define-key map (kbd "o")       #'projectile-multi-occur)
    (define-key map (kbd "p")       #'projectile-switch-project)
    (define-key map (kbd "q")       #'projectile-switch-open-project)
    (define-key map (kbd "r")       #'projectile-replace)
    (define-key map (kbd "R")       #'projectile-regenerate-tags)
    (define-key map (kbd "s g")     #'projectile-grep)
    (define-key map (kbd "s r")     #'projectile-ripgrep)
    (define-key map (kbd "s S")     #'deadgrep)
    (define-key map (kbd "s s")     #'consult-ripgrep)
    (define-key map (kbd "S")       #'projectile-save-project-buffers)
    (define-key map (kbd "t")       #'projectile-toggle-between-implementation-and-test)
    (define-key map (kbd "T")       #'projectile-find-test-file)
    (define-key map (kbd "v")       #'projectile-vc)
    (define-key map (kbd "V")       #'projectile-browse-dirty-projects)
    (define-key map (kbd "C")       #'projectile-configure-project)
    (define-key map (kbd "c")       #'projectile-compile-project)
    (define-key map (kbd "K")       #'projectile-package-project)
    (define-key map (kbd "L")       #'projectile-install-project)
    (define-key map (kbd "P")       #'projectile-test-project)
    (define-key map (kbd "u")       #'projectile-run-project)
    (define-key map (kbd "x e")     #'projectile-run-eshell)
    (define-key map (kbd "x i")     #'projectile-run-ielm)
    (define-key map (kbd "x t")     #'projectile-run-term)
    (define-key map (kbd "x s")     #'projectile-run-shell)
    (define-key map (kbd "x g")     #'projectile-run-gdb)
    (define-key map (kbd "x v")     #'projectile-run-vterm)
    (define-key map (kbd "z")       #'projectile-cache-current-file)
    (define-key map (kbd "<left>")  #'projectile-previous-project-buffer)
    (define-key map (kbd "<right>") #'projectile-next-project-buffer)
    (define-key map (kbd "ESC")     #'projectile-project-buffers-other-buffer)
    map))
(fset 'my/projectile-command-map my/projectile-command-map)

(my/package projectile
  :straight t

  :commands (projectile-locate-dominating-file
             projectile-project-name
             projectile-project-p
             projectile-project-root
             projectile-switch-project-by-name)

  :init
  (general-def
    [remap evil-jump-to-tag] #'projectile-find-tag
    [remap find-tag]         #'projectile-find-tag)

  (general-def
    :keymaps 'override
    :states 'normal
    :prefix my/leader
    "p" `(:keymap my/projectile-command-map :package projectile))

  (setq projectile-cache-file (concat my/cache-dir "projectile.cache"))
  (setq projectile-enable-caching nil)
  (setq projectile-globally-ignored-file-suffixes '(".elc" ".pyc" ".o"))
  (setq projectile-globally-ignored-files '(".DS_Store" "TAGS"))
  (setq projectile-ignored-projects `("~/" "/tmp" ,my/local-dir))
  (setq projectile-kill-buffers-filter 'kill-only-files)
  (setq projectile-known-projects-file (concat my/cache-dir "projectile-bookmarks.eld"))
  (setq projectile-project-search-path '("~/data" "~/data/repos"))
  (setq projectile-auto-discover nil) ; This can be slow, so do it manually
  (setq projectile-require-project-root t)
  (setq projectile-completion-system 'auto)

  ;; It's recommended to use fd as a replacement for both git ls-files and find.
  (setq projectile-generic-command "fd . --color=never --type f -0 -H -E .git")
  (setq projectile-git-command projectile-generic-command)

  ;; Skip warnings about unsafe variables in .dir-locals.el
  (put 'projectile-project-type 'safe-local-variable #'symbolp)

  ;; Always open the top-level project directory after switching projects.
  (setq projectile-switch-project-action #'projectile-dired)

  :config
  (projectile-mode +1))

(provide 'pkg-projectile)
