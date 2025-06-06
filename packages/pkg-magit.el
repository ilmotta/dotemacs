;;; -*- lexical-binding: t; -*-

;;; Code:

(require 'lib-util)

(defvar-local pkg-magit/main-branch "origin/main"
  "Main repo branch name.
Main here means the repo's most common branch pull-requests are
merged into. Prefer to set this value on a directory local
variable.")

(defvar pkg-magit/command-map
  (define-keymap
    "d m" #'pkg-magit/diff-from-main
    "d r" #'pkg-magit/diff-from
    "d f" #'magit-diff-buffer-file
    "s"   #'magit-status
    "l"   #'magit-log-current
    "L"   #'pkg-magit/log-dwim
    "c A" #'pkg-magit/auto-commit))
(fset 'pkg-magit/command-map pkg-magit/command-map)

;;;; Autoloads

;;;###autoload
(defun pkg-magit/log-dwim ()
  (interactive)
  (if (equal major-mode 'dired-mode)
      (magit-dired-log 'follow)
    (magit-log-buffer-file 'follow)))

;;;###autoload
(defun pkg-magit/diff-from (branch-or-rev)
  "Show differences between HEAD and BRANCH-OR-REV."
  (interactive (list (magit-read-other-branch-or-commit "Diff from")))
  (magit-diff-range branch-or-rev))

;;;###autoload
(defun pkg-magit/diff-from-main ()
  "Show differences between HEAD and `pkg-magit/main-branch'."
  (interactive)
  (pkg-magit/diff-from pkg-magit/main-branch))

;;;###autoload
(defun pkg-magit/auto-commit ()
  "Stage all files and commit with current timestamp."
  (interactive)
  (let* ((buffer "*auto-commit*")
         (timestamp (format-time-string "%Y-%m-%d %H:%M:%S" (current-time)))
         (cmd (format "git add --all && git commit -m '%s'" timestamp))
         (proc (start-process-shell-command "git" buffer cmd)))
    (set-process-sentinel
     proc
     (lambda (process _event)
       (let ((status (process-exit-status process)))
         (cond ((and (eq 'exit (process-status process)) (zerop status))
                (message "Auto-committed successfully.")
                (magit-refresh-all)
                (kill-buffer buffer))
               (t
                (pop-to-buffer buffer))))))))

(defun pkg-magit/diff-visit-file (original-fn &rest args)
  "Try to visit file reusing buffer window and recenter."
  (lib-util/with-buf-reuse-window
   (apply original-fn args)
   (recenter)))

(defun pkg-magit/display-buffer-same-window (buffer)
  (display-buffer buffer '(display-buffer-same-window)))

;; Set to nil to not bind to C-x. This variable must be set before the call to
;; `use-package'.
;; (setq magit-define-global-key-bindings nil)

;; We get errors in Emacs 29 saying package git-commit could not be found.
(when (< emacs-major-version 30)
  (lib-util/pkg git-commit
    :elpaca (:host github
             :repo "magit/magit"
             :ref "f44f6c14500476d918e9c01de8449edb20af4113"
             :files ("lisp/git-*.el"))))

(defmacro pkg-magit/use-package ()
  `(lib-util/pkg magit
     :elpaca ,(if (>= emacs-major-version 30)
                  '(:host github
                    :repo "magit/magit"
                    :ref "4992c3d1f64e0e983692c7a61d47069f47380dbf"
                    :files ("lisp/magit*.el" "lisp/git-*.el" "docs/magit.texi" "docs/AUTHORS.md" "LICENSE" ".dir-locals.el" (:exclude "lisp/magit-section.el")))
                '(:host github
                  :repo "magit/magit"
                  :ref "f44f6c14500476d918e9c01de8449edb20af4113"
                  :files ("lisp/magit*.el"
                          "lisp/git-*.el"
                          "docs/magit.texi"
                          "docs/AUTHORS.md"
                          "LICENSE"
                          ".dir-locals.el"
                          (:exclude "lisp/magit-section.el"))))
     :defer t

     :commands (magit-log-current
                magit-status
                pkg-magit/auto-commit
                pkg-magit/log-dwim)

     ;; Enable spell check automatically.
     :hook (git-commit-mode-hook . flyspell-mode)

     :init
     (general-def
       :keymaps 'my/keys-mode-map
       :states 'normal
       :prefix my/leader
       "g" #'pkg-magit/command-map)

     ;; "f" is used by the translation map to mean C-x. Since I use evil, M-f
     ;; (`forward-word') is available to me.
     (general-def
       :keymaps 'magit-mode-map
       "f" nil
       "M-f" #'magit-fetch)

     ;; Show diffs on the same window. Use
     ;; `magit-display-buffer-same-window-except-diff-v1' if you prefer to be able
     ;; to see diffs side-by-side.
     ;; (setq magit-display-buffer-function #'pkg-magit/display-buffer-same-window)
     ;;
     (setq magit-display-buffer-function #'magit-display-buffer-same-window-except-diff-v1)
     (setq magit-commit-show-diff t)

     ;; Improve diff performance.
     (setq magit-diff-adjust-tab-width nil
      magit-diff-hide-trailing-cr-characters nil
      magit-diff-highlight-hunk-body t
      magit-diff-highlight-indentation nil
      magit-diff-highlight-keywords t
      magit-diff-highlight-trailing t
      magit-diff-paint-whitespace nil)

     ;; Show fine differences in all diff hunks. This is performing way better in
     ;; general than setting the value to t.
     (setq magit-diff-refine-hunk t)

     (setq git-commit-summary-max-length 70
      git-commit-style-convention-checks '(non-empty-second-line
                                           overlong-summary-line))
     :config
     ;; When refreshing the "references buffer" is slow, then that’s usually
     ;; because several hundred refs are being displayed. The best way to address
     ;; that is to display fewer refs, obviously. If you are not, or only mildly,
     ;; interested in seeing the list of tags, then start by not displaying them:
     (remove-hook 'magit-refs-sections-hook #'magit-insert-tags)

     (advice-add #'magit-diff-visit-file :around #'pkg-magit/diff-visit-file)

     ;; Do not show "Recent commits" section.
     (remove-hook 'magit-status-sections-hook #'magit-insert-unpushed-to-upstream-or-recent)

     ;; I don't use stashes that much, and if I do, it's just 'z l' keybinding
     ;; away.
     (remove-hook 'magit-status-sections-hook #'magit-insert-stashes)))

(pkg-magit/use-package)

(provide 'pkg-magit)
