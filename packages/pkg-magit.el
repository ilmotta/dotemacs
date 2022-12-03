;;; -*- lexical-binding: t; -*-

;;; Code:

(defvar pkg-magit/command-map
  (define-keymap
    "s" #'magit-status
    "l" #'magit-log-current
    "L" #'pkg-magit/log-dwim
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
  (my/with-buffer-reuse-window
   (apply original-fn args)
   (recenter)))

(my/package magit
  :straight t
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
    :states '(normal insert emacs)
    :prefix my/leader
    :non-normal-prefix "C-x"
    "g" #'pkg-magit/command-map)

  (setq magit-define-global-key-bindings nil)

  ;; If a buffer's major-mode derives from magit-diff-mode or magit-process-mode,
  ;; display it in another window. Display all other buffers in the selected
  ;; window.
  (setq magit-display-buffer-function #'magit-display-buffer-same-window-except-diff-v1)

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

  (setq git-commit-summary-max-length 50
        git-commit-style-convention-checks '(non-empty-second-line
                                             overlong-summary-line))
  :config
  ;; When refreshing the "references buffer" is slow, then that’s usually
  ;; because several hundred refs are being displayed. The best way to address
  ;; that is to display fewer refs, obviously. If you are not, or only mildly,
  ;; interested in seeing the list of tags, then start by not displaying them:
  (remove-hook 'magit-refs-sections-hook #'magit-insert-tags)

  (advice-add #'magit-diff-visit-file :around #'pkg-magit/diff-visit-file)

  ;; DON'T show the recent commits section.
  ;; (remove-hook 'magit-status-sections-hook #'magit-insert-unpushed-to-upstream-or-recent)

  ;; "C-g" is the default binding for such commands now, but Transient's
  ;; predecessor Magit-Popup used "q" instead.
  (transient-bind-q-to-quit))

(provide 'pkg-magit)
