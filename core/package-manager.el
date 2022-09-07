;;; -*- lexical-binding: t; -*-

;;; Code:

;; Shallow clone repositories. Note: Unfortunately, shallow clone doesn't work
;; well because the freeze and thaw commands fail.
;;
;; (setq straight-vc-git-default-clone-depth 1)

;; You'll need to manually call `straight-rebuild-all' or
;; `straight-rebuild-package'.
(setq straight-check-for-modifications nil)

;; Straight.el will skip the following packages instead of looking for a recipe.
;; The ones that are not bundled with Emacs have a comment.
(setq straight-built-in-pseudo-packages
      '(battery
        diff
        dired
        emacs
        emacs-lisp-mode
        epa
        eshell
        goto-addr
        hl-line
        ispell
        proced
        project
        python
        recentf
        savehist
        saveplace
        shell-mode
        tempo
        time
        timeclock
        tramp
        vterm ; Installed via NixOS
        winner
        xref))

;; Files that can be discarded should live in the local directory.
(setq straight-base-dir my/local-dir)

;; Tell straight.el to use the lock file as a profile instead of
;; straight/versions/default.el.
(setq straight-profiles `((nil . ,(concat user-emacs-directory "package-lock.el"))))

;; Configure `use-package' prior to loading it. The *important* piece here is
;; that the hook name suffix feature is disabled, which in turn affects all
;; :hook properties. The motivation to disable this setting is that /help
;; commands/ will be able to show more useful information.
(eval-and-compile
  (setq use-package-always-defer nil
        use-package-always-demand nil
        use-package-expand-minimally nil
        use-package-hook-name-suffix nil))

;; Finally bootstrap straight.el.
(let ((bootstrap-version 5)
      (bootstrap-file (expand-file-name ".local/straight/repos/straight.el/bootstrap.el"
                                        user-emacs-directory)))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        ;; Branch "develop", committed on 2022-05-13.
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/radian-software/straight.el/99ba608ed85e8814d89f00e09f3d99d76ee4f3d3/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

;; Auto-install `use-package'.
(straight-use-package 'use-package)

;; Compute statistics for `use-package' declarations. You can view the
;; statistical report using `use-package-report'.
(setq use-package-compute-statistics nil)
