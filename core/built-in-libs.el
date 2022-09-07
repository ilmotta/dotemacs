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
