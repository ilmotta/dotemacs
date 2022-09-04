;;; -*- lexical-binding: t; -*-

(defgroup built-in-libs nil
  "Configuration for built-in packages."
  :group 'my
  :prefix "built-in-libs/")

;;; Auto-revert

;; Only prompts for confirmation when buffer is unsaved.
(setq revert-without-query '("."))

(setq auto-revert-verbose t
      auto-revert-stop-on-user-input nil
      auto-revert-use-notify nil)

;;; Battery

(setq battery-mode-line-limit 99
      battery-update-interval 60
      battery-load-low 20
      battery-load-critical 10)

;;; Bookmark

;; File in which to save bookmarks by default.
(setq bookmark-default-file (concat my/cache-dir "bookmarks"))

(setq bookmark-set-fringe-mark t)

;;; Compilation

;; Don't ask for the compilation command in the minibuffer. I often prefer to
;; set this variable in a directory locals file.
(setq compilation-read-command nil)

;;; Diff

(setq diff-default-read-only t)

;;; Eldoc

(setq eldoc-idle-delay 0.25
      eldoc-minor-mode-string nil
      eldoc-echo-area-use-multiline-p nil)

;;; Eshell
;;;; Faces

(defface pkg-eshell/prompt-pwd '((t (:inherit font-lock-constant-face)))
  "Face for current working directory."
  :group 'eshell)

(defface pkg-eshell/prompt-git-branch '((t (:inherit font-lock-builtin-face)))
  "Face for git branch."
  :group 'eshell)

;;;; Custom

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

;;;; State

(defvar pkg-eshell/-last-buffer nil)

(defvar pkg-eshell/buffers (make-ring 25)
  "List of open eshell buffers.")

;;;; Private

(defcustom pkg-eshell/buffer-name-separator " • "
  "String used to separate `eshell-buffer-name' and the current
working directory (`default-directory')."
  :type 'string
  :group 'eshell)

(defun pkg-eshell/last-input ()
  (when (and eshell-last-input-start eshell-last-input-end)
    (let ((input (string-trim-right (buffer-substring-no-properties eshell-last-input-start eshell-last-input-end))))
      (if (string-blank-p input)
          nil
        input))))

(defun pkg-eshell/rename-buffer-with-last-input ()
  "Rename current eshell buffer with last input."
  (when-let ((last-input (pkg-eshell/last-input)))
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

(defun pkg-eshell/-buffers ()
  (ring-elements pkg-eshell/buffers))

(defun pkg-eshell/-bury-buffer ()
  (unless (switch-to-prev-buffer nil 'bury)
    (switch-to-buffer "*untitled*"))
  (when (eq major-mode 'eshell-mode)
    (switch-to-buffer "*untitled*")))

(defun pkg-eshell/-unused-buffer (&optional new-p)
  (or (unless new-p
        (cl-loop for buf in (pkg-eshell/-buffers)
                 if (and (buffer-live-p buf)
                         (not (get-buffer-window buf t)))
                 return buf))
      (generate-new-buffer eshell-buffer-name)))

(defun pkg-eshell/-buffer-name (path)
  (generate-new-buffer-name (format "*eshell %s*" (shrink-path-file path))))

(defun pkg-eshell/-in-project-p (project-path)
  (lambda (buf)
    (equal (expand-file-name project-path)
           (with-current-buffer buf
             (expand-file-name (project-root (project-current)))))))

(defun pkg-eshell/-project-unused-buffer-name (&optional new-p)
  (if-let (this-buf-root (project-root (project-current)))
      (if new-p
          (pkg-eshell/-buffer-name this-buf-root)
        (or (thread-last (pkg-eshell/-buffers)
                         (seq-filter #'buffer-live-p)
                         (seq-filter (pkg-eshell/-in-project-p this-buf-root))
                         (seq-map #'buffer-name)
                         (seq-first))
            (pkg-eshell/-buffer-name this-buf-root)))
    (thread-last (pkg-eshell/-buffers)
                 (seq-filter #'buffer-live-p)
                 (seq-map #'buffer-name)
                 (seq-first))))

(defun pkg-eshell/-add-buffer (buf)
  (ring-remove+insert+extend pkg-eshell/buffers buf 'grow))

(defun pkg-eshell/-remove-buffer (buf)
  (when-let (idx (ring-member pkg-eshell/buffers buf))
    (ring-remove pkg-eshell/buffers idx)
    t))

(defun pkg-eshell/-setup-window (window &optional flag)
  (when (window-live-p window)
    (set-window-parameter window 'no-other-window flag)
    (set-window-parameter window 'visible flag)))

(defun pkg-eshell/init-h ()
  "Initialize and track this eshell buffer in `pkg-eshell/buffers'."
  ;; Let `consult-outline' do its magic.
  (setq outline-regexp eshell-prompt-regexp)

  (let ((current-buffer (current-buffer)))
    (dolist (buf (pkg-eshell/-buffers))
      (unless (buffer-live-p buf)
        (pkg-eshell/-remove-buffer buf)))
    (pkg-eshell/-setup-window (get-buffer-window current-buffer))
    (pkg-eshell/-add-buffer current-buffer)
    (setq pkg-eshell/-last-buffer current-buffer)))

(defun pkg-eshell/cleanup-h ()
  "Close window on quit."
  (let ((buf (current-buffer)))
    (when (pkg-eshell/-remove-buffer buf)
      (when-let (win (get-buffer-window buf))
        (pkg-eshell/-setup-window win nil)))))

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

(defun pkg-eshell/-prompt-p ()
  (let ((line (buffer-substring-no-properties (line-beginning-position) (line-end-position))))
    (string-match-p eshell-prompt-regexp line)))

(defun pkg-eshell/-setup-buffer (buffer-or-name &optional command)
  (let ((buf (get-buffer-create buffer-or-name)))
    (with-current-buffer buf
      ;; `eshell-mode' can't be called when the major mode is already set to
      ;; eshell because it will insert a new prompt.
      (if (eq major-mode 'eshell-mode)
          (run-hooks 'eshell-mode-hook)
        (eshell-mode))
      (when command
        (pkg-eshell/run-command command buf)))
    buf))

;;;; Autoloads

;;;###autoload
(defun pkg-eshell/run-command (command &optional buffer)
  (let ((buffer
         (or buffer
             (if (eq major-mode 'eshell-mode)
                 (current-buffer)
               (cl-find-if #'buffer-live-p (pkg-eshell/-buffers))))))
    (unless buffer
      (user-error "No living eshell buffers available"))
    (unless (buffer-live-p buffer)
      (user-error "Cannot operate on a dead buffer"))
    (with-current-buffer buffer
      (goto-char eshell-last-output-end)
      (goto-char (line-end-position))
      (insert command)
      (eshell-send-input nil t))))

;;;###autoload
(defun pkg-eshell/here ()
  "Open eshell in the current window."
  (interactive)
  (thread-first (pkg-eshell/-project-unused-buffer-name)
                pkg-eshell/-setup-buffer
                switch-to-buffer))

;;;###autoload
(defun pkg-eshell/split-below ()
  "Create a new eshell window below the current one."
  (interactive)
  (let ((ignore-window-parameters t))
    (select-window (split-window-vertically))
    (thread-first (pkg-eshell/-project-unused-buffer-name 'new)
                  pkg-eshell/-setup-buffer
                  switch-to-buffer)))

;;;###autoload
(defun pkg-eshell/split-right ()
  "Create a new eshell window to the right of the current one."
  (interactive)
  (let* ((ignore-window-parameters t))
    (select-window (split-window-horizontally))
    (thread-first (pkg-eshell/-project-unused-buffer-name 'new)
                  pkg-eshell/-setup-buffer
                  switch-to-buffer)))

;;;###autoload
(defun pkg-eshell/toggle (arg)
  "Toggle eshell popup window. With prefix ARG delete the buffer window."
  (interactive "P")
  (let* ((buf (pkg-eshell/-project-unused-buffer-name))
         (confirm-kill-processes nil)
         (win (get-buffer-window buf)))
    (if arg
        (when win
          (if (one-window-p 'no-miniframe)
              (pkg-eshell/-bury-buffer)
            (delete-window win)))
      (if win
          (if (one-window-p 'no-miniframe)
              (pkg-eshell/-bury-buffer)
            (delete-window win))
        (thread-first buf pkg-eshell/-setup-buffer pop-to-buffer)))))

;;;###autoload
(defun pkg-eshell/project-toggle ()
  "Toggle eshell popup window."
  (interactive)
  (let* ((buf-name (pkg-eshell/-project-unused-buffer-name))
         (confirm-kill-processes nil)
         (win (get-buffer-window buf-name)))
    (if win
        (if (one-window-p 'no-miniframe)
            (pkg-eshell/-bury-buffer)
          (delete-window win))
      (let ((default-directory (or (project-root (project-current)) default-directory)))
        (thread-first buf-name pkg-eshell/-setup-buffer pop-to-buffer)))))

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

;;;; Init

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

;;;; Config

(add-hook 'eshell-mode-hook #'pkg-eshell/init-h)
(add-hook 'eshell-exit-hook #'pkg-eshell/cleanup-h)
(add-hook 'eshell-post-command-hook #'pkg-eshell/rename-buffer-with-last-input)

(add-to-list 'display-buffer-alist
             `(,(rx line-start "*eshell" (zero-or-more not-newline) line-end)
               (display-buffer-in-side-window)
               (window-width . 0.5)
               (side . right)
               (slot . 0)
               (body-function . select-window)))

;;; Image dired

(setq image-dired-thumb-size 150
      image-dired-thumb-margin 2
      ;; No border around thumbnails
      image-dired-thumb-relief 0)

;; Where to store image caches.
(setq image-dired-dir (concat my/cache-dir "image-dired/")
      image-dired-db-file (concat image-dired-dir "db.el")
      image-dired-gallery-dir (concat image-dired-dir "gallery/")
      image-dired-temp-image-file (concat image-dired-dir "temp-image")
      image-dired-temp-rotate-image-file (concat image-dired-dir "temp-rotate-image"))

;;; Isearch

;; Isearch has been improved in Emacs 27 and can now display match numbers in
;; the modeline.
;;
;; Highlights matches in the full buffer. It is useful in combination with
;; `lazy-highlight-cleanup' customized to nil to leave matches highlighted in
;; the whole buffer after exiting isearch.
(setq isearch-lazy-count t
      isearch-allow-scroll t
      lazy-highlight-initial-delay 0
      lazy-highlight-buffer t
      lazy-highlight-cleanup t
      lazy-highlight-buffer-max-at-a-time 10)

;;; Ispell

(when-let ((dir (getenv "ASPELL_DICT_DIR")))
  (setq ispell-extra-args `("--dict-dir" ,dir)))
(add-to-list 'ispell-skip-region-alist '(":\\(PROPERTIES\\|LOGBOOK\\):" . ":END:"))
(add-to-list 'ispell-skip-region-alist '("#\\+BEGIN_SRC" . "#\\+END_SRC"))

;;; Mail

;; Specially needed for Gmail.
(setq mail-host-address "gmail.com")

(setq user-full-name "Icaro Motta")
(setq user-mail-address "icaro.ldm@gmail.com")

;; Setup User-Agent header. It is set to `message-user-agent' which means we
;; will be using `message-mode' to compose mail.
(setq mail-user-agent 'message-user-agent)

;; Disable warnings about changes in `mail-user-agent'.
(setq compose-mail-user-agent-warnings nil)

;;;; Message

(setq message-sendmail-envelope-from 'header)

;; In case we want to sendmail with a different account than the default one.
;; (setq message-sendmail-extra-arguments '("-a" "..."))
;; (setq message-send-mail-function #'sendmail-send-it)

;; Non-nil means that the message buffer will be killed after sending a message.
(setq message-kill-buffer-on-exit t)

;; For Debians, set to nil. Non-nil means don't add "-f username" to the
;; sendmail command line.
(setq message-sendmail-f-is-evil nil)

;;;; Sendmail

;; If non-nil, specify the envelope-from address when sending mail. The value
;; used to specify it is whatever is found in the variable mail-envelope-from,
;; with user-mail-address as fallback.
(setq mail-specify-envelope-from t)

;; TODO: use msmtpq to queue sending mail.
(setq sendmail-program (executable-find "msmtp"))
(setq mail-specify-envelope-from t)
(setq mail-envelope-from 'header)

;;; Save history

;;;; Init

(setq savehist-file (concat my/cache-dir "savehist")
      savehist-save-minibuffer-history t)

;; Save on kill only.
(setq savehist-autosave-interval nil)

;; You don't need to add minibuffer history variables to this list.
(setq savehist-additional-variables
      '(search-ring regexp-search-ring))

;;;; Config

(add-hook 'after-init-hook #'savehist-mode)

;;; Save place

(defun pkg-saveplace/reposition ()
  "Force windows to recenter current line (with saved position)."
  (run-with-timer
   0 nil
   (lambda (buf)
     (when (buffer-live-p buf)
       (dolist (win (get-buffer-window-list buf nil t))
         (with-selected-window win (recenter)))))
   (current-buffer)))

;;;; Init

(setq save-place-file (concat my/cache-dir "saveplace")
      save-place-limit 100)

;;;; Config

(add-hook 'find-file-hook 'pkg-saveplace/reposition t)
(add-hook 'after-init-hook #'save-place-mode)

;;; Security, encryption
;;;; EPA

;; Disable auto-saving when opening an encrypted file.
(setq epa-file-inhibit-auto-save t)

;; Do not cache passphrase for symmetric encryption.
(setq epa-file-cache-passphrase-for-symmetric-encryption nil)

;;;; EPG

;; With gpg version 2.1+ we can avoid the GUI pinentry and enter the passphrase
;; directly in Emacs.
(setq epg-pinentry-mode 'loopback)

;;;; Password cache

(setq password-cache t)

;; How many seconds passwords are cached.
(setq password-cache-expiry 60)

;;; Show parentheses

;; By default, there’s a small delay before showing a matching parenthesis. It
;; can be deactivated with the following (which you have to do before activating
;; show-paren-mode).
(setq show-paren-delay 0
      show-paren-highlight-openparen t
      show-paren-when-point-inside-paren t
      show-paren-when-point-in-periphery t)

(add-hook 'prog-mode-hook #'show-paren-mode)

;;; Tab bar

(defun pkg-tab-bar/select-tab-action (tab-name)
  "Switch to TAB-NAME or create a new tab using TAB-NAME."
  (if (tab-bar--tab-index-by-name tab-name)
      (tab-bar-switch-to-tab tab-name)
    (tab-bar-new-tab)
    (tab-bar-rename-tab tab-name)))

;;;; Autoloads

;;;###autoload
(defun pkg-tab-bar/select-tab-dwim ()
  "Either create a tab or show completion candidates.

If the tab name doesn't exist, then create a tab with the given
name and switch to it. If there's only one other tab, then
automatically switch to it."
  (interactive)
  (let ((tabs (mapcar (lambda (tab)
                        (alist-get 'name tab))
                      (tab-bar--tabs-recent))))
    (cond
     ((eq tabs nil)
      (tab-bar-new-tab))
     ((eq (length tabs) 1)
      (tab-bar-switch-to-next-tab))
     (t
      (consult--jump
       (pkg-tab-bar/select-tab-action
        (consult--read "Select tab: "
                       tabs
                       :sort nil
                       :require-match t
                       :category 'tab)))))))

;;;; Keybindings

;; Use the "C-x t" prefix in order to be consistent with standard Emacs
;; keybindings.
(define-key my/keys-mode-map (kbd "C-x t t") #'pkg-tab-bar/select-tab-dwim)

;;;; Init

(setq tab-bar-close-button-show nil
      tab-bar-close-last-tab-choice nil ; Do nothing and show a message.
      tab-bar-close-tab-select 'recent
      tab-bar-new-tab-to 'right
      tab-bar-new-button-show nil
      tab-bar-new-tab-choice t      ; Start a new tab with the current buffer.
      tab-bar-position nil          ; Show above the tool bar.
      tab-bar-show nil              ; Always keep the tab bar hidden.
      tab-bar-tab-hints nil         ; Do not show numbers on tabs.
      tab-bar-tab-name-function #'tab-bar-tab-name-all)

;;;; Config

(tab-bar-history-mode -1)

;;; Time

;; Display time, load level, and mail flag in mode lines.

(setq display-time-format "%H:%M"
      display-time-interval 60
      display-time-mail-directory nil
      display-time-default-load-average nil)
;;; Timeclock

(setq timeclock-file (concat my/cache-dir "timelog"))
;;; Tramp

;; Value of TERM environment variable for logging in to remote host.
(setq tramp-terminal-type "tramp")

;; Set from 0 to 11 (more verbose). Level 1 means only errors will be logged.
;; Default is 3.
(setq tramp-verbose 1)

;; Set path to file which keeps connection history.
(setq tramp-persistency-file-name (concat my/cache-dir "tramp"))

;;; Transient

(setq transient-values-file (concat my/cache-dir "transient/values.el")
      transient-levels-file (concat my/cache-dir "transient/levels.el")
      transient-history-file (concat my/cache-dir "transient/history.el"))

;; If non-nil, then the key binding of each suffix is colorized to indicate
;; whether it exits the transient state or not.
;;
;; Note: The colors look odd in most themes.
(setq transient-semantic-coloring nil)

;;; URL

(setq url-configuration-directory (concat my/cache-dir "url/"))

;;; Winner
;;;; Init

(setq winner-dont-bind-my-keys t)

(setq winner-boring-buffers
      '("*Apropos*"
        "*Buffer List*"
        "*Compile-Log*"
        "*Fuzzy Completions*"
        "*Help*"
        "*Ibuffer*"
        "*cvs*"
        "*esh command on file*"
        "*inferior-lisp*"))

;;;; Config

(winner-mode +1)
