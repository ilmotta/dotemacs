;;; -*- lexical-binding: t; -*-

;;; Faces

(defface pkg-eshell/prompt-pwd '((t (:inherit font-lock-constant-face)))
  "Face for current working directory."
  :group 'eshell)

(defface pkg-eshell/prompt-git-branch '((t (:inherit font-lock-builtin-face)))
  "Face for git branch."
  :group 'eshell)

;;; Custom

(defcustom pkg-eshell/buffer-name "*eshell toggle*"
  "Buffer name to pop eshell."
  :type 'string)

(defcustom pkg-eshell/kill-window-on-exit nil
  "If non-nil, eshell will close windows along with its eshell buffers."
  :type 'string)

(defcustom pkg-eshell/enable-new-shell-on-split t
  "If non-nil, spawn a new eshell session after splitting from an
eshell buffer."
  :type 'boolean)

(defcustom pkg-eshell/buffer-name-separator " • "
  "String used to separate `eshell-buffer-name' and the current
working directory (`default-directory')."
  :type 'string
  :group 'eshell)

;;; Private

(defun pkg-eshell/-last-input ()
  (when (and eshell-last-input-start eshell-last-input-end)
    (let ((input (string-trim-right (buffer-substring-no-properties eshell-last-input-start eshell-last-input-end))))
      (if (string-blank-p input)
          nil
        input))))

(defun pkg-eshell/-current-git-branch ()
  (cl-destructuring-bind (status . output)
      (lib-util/call-process "git symbolic-ref -q --short HEAD")
    (if (equal status 0)
        (format " [%s]" output)
      (cl-destructuring-bind (status . output)
          (lib-util/call-process "git describe --all --always HEAD")
        (if (equal status 0)
            (format " [%s]" output)
          "")))))

(defun pkg-eshell/-prompt-p ()
  (let ((line (buffer-substring-no-properties (line-beginning-position) (line-end-position))))
    (string-match-p eshell-prompt-regexp line)))

;;; Public
;;;; Functions

(defun pkg-eshell/rename-buffer-with-last-input ()
  "Rename current eshell buffer with last input."
  (when-let ((last-input (pkg-eshell/-last-input)))
    (let* ((separator pkg-eshell/buffer-name-separator)
           (separator-regexp (rx (literal separator)
                                 (group (one-or-more not-newline))
                                 line-end))
           (current-name (buffer-name))
           (new-name (generate-new-buffer-name (if (string-match-p separator-regexp current-name)
                                                   (replace-regexp-in-string separator-regexp
                                                                             (concat separator last-input)
                                                                             current-name)
                                                 (concat current-name separator last-input)))))
      (rename-buffer new-name))))

(defun pkg-eshell/default-prompt ()
  "Generate the prompt string for eshell. Use for `eshell-prompt-function'."
  (concat (if (bobp) "" "\n")
          (let ((pwd (eshell/pwd)))
            (propertize (if (equal pwd "~")
                            pwd
                          (abbreviate-file-name (shrink-path-file pwd)))
                        'face 'pkg-eshell/prompt-pwd))
          (propertize (pkg-eshell/-current-git-branch)
                      'face 'pkg-eshell/prompt-git-branch)
          (propertize " λ" 'face (if (zerop eshell-last-command-status) 'success 'error))
          " "))

;;;; Autoloads

;;;###autoload
(defun pkg-eshell/clear ()
  (interactive)
  (eshell/clear-scrollback)
  (eshell-emit-prompt))

;;;###autoload
(defun pkg-eshell/copy-input ()
  (interactive)
  (if (pkg-eshell/-prompt-p)
      (kill-new (eshell-get-old-input))
    (save-excursion
      (eshell-previous-prompt 1)
      (kill-new (eshell-get-old-input)))))

;;;###autoload
(defun pkg-eshell/copy-output ()
  (interactive)
  (kill-new (buffer-substring-no-properties (eshell-beginning-of-output)
                                            (eshell-end-of-output))))

;;;###autoload
(defun pkg-eshell/copy-dwim ()
  (interactive)
  (if (pkg-eshell/-prompt-p)
      (pkg-eshell/copy-input)
    (pkg-eshell/copy-output)))

;;;###autoload
(defun pkg-eshell/change-line ()
  (interactive)
  (if (pkg-eshell/-prompt-p)
      (progn
        (eshell-bol)
        (evil-change-line (point) (line-end-position)))
    (evil-change-whole-line)))


(lib-util/pkg eshell
  :elpaca nil

  :hook (eshell-post-command-hook . pkg-eshell/rename-buffer-with-last-input)

  :init
  (setq eshell-banner-message "")
  (setq eshell-directory-name (concat my/cache-dir "eshell/"))
  (setq eshell-kill-processes-on-exit t)
  (setq eshell-scroll-to-bottom-on-input 'all)
  (setq eshell-scroll-to-bottom-on-output 'all)
  (setq eshell-history-size 10000)

  ;; If non-nil, term buffers are destroyed after their processes die. WARNING:
  ;; Setting this to non-nil may result in unexpected behavior for short-lived
  ;; processes, see bug#18108.
  (setq eshell-destroy-buffer-when-process-dies nil)

  ;; Uncomment to remove all escape sequences. Default is nil.
  ;; (setq eshell-preoutput-filter-functions '(ansi-color-filter-apply))
  (setq eshell-preoutput-filter-functions nil)

  (setq eshell-output-filter-functions
        '(eshell-postoutput-scroll-to-bottom
          eshell-handle-control-codes
          eshell-handle-ansi-color
          eshell-watch-for-password-prompt))

  ;; Example, use Eshell's sudo.
  (setq eshell-prefer-lisp-functions t
        eshell-prefer-lisp-variables t)

  (setq eshell-save-history-on-exit t)
  (setq eshell-hist-ignoredups t)

  (setq eshell-modules-list
        '(eshell-alias
          ;; eshell-banner
          eshell-basic
          eshell-cmpl
          eshell-dirs
          eshell-glob
          eshell-hist
          eshell-ls
          eshell-pred
          eshell-prompt
          eshell-script
          eshell-term
          ;; Required to have eshell/sudo available, which in turn supports
          ;; password caching. Unfortunately, eshell/sudo doesn't work properly.
          ;; For example, it blocks Emacs while the sudo command is running.
          ;;
          ;; eshell-tramp
          eshell-unix))

  ;; Do not add input beginning with empty space to history.
  (setq eshell-input-filter #'eshell-input-filter-initial-space)

  (setq eshell-glob-case-insensitive t)

  ;; Visual commands require a proper terminal. Eshell can't handle that, so it
  ;; delegates these commands to a term buffer.
  (setq eshell-visual-commands '("bat"
                                 "htop"
                                 "less"
                                 "more"
                                 "ncmpcpp"
                                 "screen"
                                 "tig"
                                 "tmux"
                                 "top"
                                 "vi"
                                 "vim"))

  ;; For exa colored output, consider the answer from
  ;; https://emacs.stackexchange.com/questions/51027/missing-color-support-for-exa-in-eshell.
  (setq eshell-command-aliases-list
        '(("q"  "exit")
          ("f"  "find-file $1")
          ("d"  "dired $1")
          ("l" "exa -lg --sort=name --group-directories-first --time-style long-iso --git")
          ("ll" "exa -lag --sort=name --group-directories-first --time-style long-iso --git $*")
          ("git" "git --no-pager $*")
          ("gs" "magit-status")
          ("clear" "clear-scrollback")))

  (setq eshell-prompt-regexp "^.* λ ")
  (setq eshell-prompt-function #'pkg-eshell/default-prompt)

  ;; It is an error for a glob pattern not to match. This mimics the behavior of
  ;; zsh if non-nil, but bash if nil.
  (setq eshell-error-if-no-glob t)

  :config
  (add-to-list 'display-buffer-alist
               `(,(rx line-start "*eshell" (zero-or-more not-newline) line-end)
                 (display-buffer-in-side-window)
                 (window-width . 0.5)
                 (side . right)
                 (slot . 0)
                 (body-function . select-window))))

(provide 'pkg-eshell)
