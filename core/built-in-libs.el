;;; -*- lexical-binding: t; -*-

(defgroup built-in-libs nil
  "Configuration for built-in packages."
  :group 'my
  :prefix "built-in-libs/")

;;; Diff

(setq diff-default-read-only t)

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
