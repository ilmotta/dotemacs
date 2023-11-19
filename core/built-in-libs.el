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

;;; File place persistence

(defun pkg-saveplace/reposition ()
  "Force windows to recenter current line (with saved position)."
  (run-with-timer
   0 nil
   (lambda (buf)
     (when (buffer-live-p buf)
       (dolist (win (get-buffer-window-list buf nil t))
         (with-selected-window win (recenter)))))
   (current-buffer)))

(add-hook 'elpaca-after-init-hook #'save-place-mode)
(add-hook 'find-file-hook 'pkg-saveplace/reposition t)

(setq save-place-file (concat my/cache-dir "saveplace"))
(setq save-place-limit 100)

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

;;; Minibuffer history

(add-hook 'elpaca-after-init-hook #'savehist-mode)

(setq savehist-file (concat my/cache-dir "savehist"))
(setq savehist-save-minibuffer-history t)

;; Save on kill only.
(setq savehist-autosave-interval nil)

;; We don't need to add minibuffer history variables to this list.
(setq savehist-additional-variables '(search-ring regexp-search-ring))

;;; Programming languages, tree-sitter

;; Maximum amount of syntax highlighting. Level 3 is too limited for JSON.
(setq-default treesit-font-lock-level 4)

;; Some .so files can be downloaded/built from
;; https://github.com/casouri/tree-sitter-module/releases.
(setq treesit-extra-load-path
      (list (concat my/local-dir "tree-sitter-grammars/")))

;;;; Javascript

(defun pkg-js-mode/setup-sibling-rules ()
  (setq-local find-sibling-rules
              (list
               ;; Source -> unit test
               (list (rx (group (+ (not "/")))
                         "." (group (or "js" "jsx" "ts" "tsx"))
                         string-end)
                     (rx (regex "\\1")
                         ".test." (regex "\\2")
                         string-end))

               ;; Unit test -> source
               (list (rx (group (+ (not "/")))
                         ".test." (group (or "js" "jsx" "ts" "tsx"))
                         string-end)
                     (rx (regex "\\1")
                         "." (regex "\\2")
                         string-end))

               ;; Source -> spec test
               (list (rx (group (+ (not "/")))
                         "." (group (or "js" "jsx" "ts" "tsx"))
                         string-end)
                     (rx (regex "\\1")
                         ".spec." (regex "\\2")
                         string-end))

               ;; Spec test -> source
               (list (rx (group (+ (not "/")))
                         ".spec." (group (or "js" "jsx" "ts" "tsx"))
                         string-end)
                     (rx (regex "\\1")
                         "." (regex "\\2")
                         string-end)))))

(add-hook 'js-mode-hook #'js-ts-mode)
(add-hook 'js-mode-hook #'pkg-js-mode/setup-sibling-rules)
(add-hook 'typescript-tsx-mode-hook #'pkg-js-mode/setup-sibling-rules)

;;;; CSS

(add-to-list 'auto-mode-alist
             (cons (rx "." (or "css") string-end)
                   'css-ts-mode))

;;;; JSON

(add-to-list 'auto-mode-alist
             (cons (rx "." (or "json") string-end)
                   'json-ts-mode))

;;;; XML

(add-to-list 'auto-mode-alist
             (cons (rx "." (or "pom" "plist" "rss" "xml" "xsd" "xslt") string-end)
                   'nxml-mode))

;; Typing a slash automatically completes the end-tag.
(setq nxml-slash-auto-complete-flag t)
(setq nxml-auto-insert-xml-declaration-flag nil)

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
      show-paren-when-point-in-periphery t

      ;; This new setting in Emacs 29.1 doesn't work well with Evil.
      show-paren-context-when-offscreen nil)

(add-hook 'prog-mode-hook #'show-paren-mode)

;;; Snippets
;;;; Commentary

;; Tempo is a powerful snippet expansion system. For examples and documentation:
;;
;; - https://www.emacswiki.org/emacs/TempoSnippets
;; - http://www.lysator.liu.se/~davidk/elisp/tempo-examples.html

;;;; Variables

(defvar-local pkg-tempo/tags-emacs-lisp nil)

;;;; Private

(defun pkg-tempo/setup-emacs-lisp ()
  (tempo-define-template
   "emacs-lisp-defun"
   '("(defun " p " (" p ")" n>
     "\"" p "\""
     n> r ")")
   "defun"
   "Insert a defun expression."
   'pkg-tempo/tags-emacs-lisp)

  (tempo-use-tag-list 'pkg-tempo/tags-emacs-lisp))

(defun pkg-tempo/setup ()
  (pkg-tempo/setup-emacs-lisp)

  ;; Catch-all default regex.
  (setq-default tempo-match-finder
                (rx line-start
                    (group (one-or-more not-newline))
                    point)))

;;;; Init

(add-hook 'emacs-lisp-mode-hook #'pkg-tempo/setup)

;;; Time

;; Display time, load level, and mail flag in mode lines.

(setq display-time-format "%H:%M"
      display-time-interval 60
      display-time-mail-directory nil
      display-time-default-load-average nil)

;;; Timeclock

(setq timeclock-file (concat my/cache-dir "timelog"))

;;; URL

(setq url-configuration-directory (concat my/cache-dir "url/"))

;;; Daemon
;;;; Private

(defun pkg-daemon/-after-make-frame-functions-h (frame)
  (when (daemonp)
    (select-frame frame)
    (if (window-system frame)
        (unless my/theme-window-loaded
          (if my/theme-terminal-loaded
              (enable-theme my/theme)
            (load-theme my/theme 'no-confirm))
          (setq my/theme-window-loaded t))
      (unless my/theme-terminal-loaded
        (if my/theme-window-loaded
            (enable-theme my/theme)
          (load-theme my/theme 'no-confirm))
        (setq my/theme-terminal-loaded t)))))

(defun pkg-daemon/-inhibit-emacsclient-message ()
  "Inhibit initial emacsclient frame message.

Remove initial message 'When done with this frame type C-x 5 0'
when starting with 'emacsclient --create-frame' (the original
solution is in
https://emacs.stackexchange.com/questions/44883/remove-emacsclient-startup-message)."
  (setq inhibit-message t)
  (run-with-idle-timer 0 nil (lambda () (setq inhibit-message nil))))

;;;; Init

(add-hook 'server-after-make-frame-hook #'pkg-daemon/-inhibit-emacsclient-message)
(add-hook 'after-make-frame-functions #'pkg-daemon/-after-make-frame-functions-h)
