;;; -*- lexical-binding: t; -*-

;;; Commentary:
;; Archived on 2023-10-19 because it's buggy. I tried integrating with it, but
;; some functions wouldn't work as expected, like `detached-kill-session'.

;;; Code:

(require 'lib-util)

;;; Consult sources

(defun pkg-detached/consult--filter-candidates (predicate)
  (thread-last (detached-session-candidates (detached-get-sessions))
               (seq-filter (lambda (candidate)
                             (funcall predicate (cdr candidate))))
               (seq-map #'car)))

(defvar pkg-detached/consult--source-hidden-session
  `(:narrow (?\s . "Hidden")
    :hidden t
    :category detached
    :annotate detached-session-annotation
    :action (lambda (x) (detached-open-session (detached--decode-session x)))
    :items
    ,(lambda ()
       (pkg-detached/consult--filter-candidates
        (lambda (session)
          (seq-find (lambda (predicate)
                      (apply predicate `(,session)))
           detached-consult-hidden-predicates)))))
  "Active `detached' sessions as a source for `consult'.")

(defvar pkg-detached/consult--source-active-session
  `(:narrow (?a . "Active")
    :hidden t
    :category detached
    :annotate detached-session-annotation
    :action (lambda (x) (detached-open-session (detached--decode-session x)))
    :items
    ,(lambda ()
       (pkg-detached/consult--filter-candidates #'detached-session-active-p)))
  "Active `detached' sessions as a source for `consult'.")

(defvar pkg-detached/consult--source-inactive-session
  `(:narrow (?i . "Inactive")
    :hidden t
    :category detached
    :annotate detached-session-annotation
    :action (lambda (x) (detached-open-session (detached--decode-session x)))
    :items
    ,(lambda ()
       (pkg-detached/consult--filter-candidates #'detached-session-inactive-p)))
  "Inactive `detached' sessions as a source for `consult'.")

(defvar pkg-detached/consult--source-failure-session
  `(:narrow (?f . "Failure")
    :hidden t
    :category detached
    :annotate detached-session-annotation
    :action (lambda (x) (detached-open-session (detached--decode-session x)))
    :items
    ,(lambda ()
       (pkg-detached/consult--filter-candidates #'detached-session-failed-p)))
  "Failed `detached' sessions as a source for `consult'.")

(defvar pkg-detached/consult--source-success-session
  `(:narrow (?s . "Success")
    :hidden t
    :category detached
    :annotate detached-session-annotation
    :action (lambda (x) (detached-open-session (detached--decode-session x)))
    :items
    ,(lambda ()
       (pkg-detached/consult--filter-candidates
        (lambda (session)
          (not (detached-session-failed-p session))))))
  "Successful `detached' sessions as a source for `consult'.")

(defvar pkg-detached/consult--source-local-session
  `(:narrow (?l . "Local Host")
    :hidden t
    :category detached
    :annotate detached-session-annotation
    :action (lambda (x) (detached-open-session (detached--decode-session x)))
    :items
    ,(lambda ()
       (pkg-detached/consult--filter-candidates #'detached-session-localhost-p))
    "Local host `detached' sessions as a source for `consult'."))

(defvar pkg-detached/consult--source-remote-session
  `(:narrow (?r . "Remote Host")
    :hidden t
    :category detached
    :annotate detached-session-annotation
    :action (lambda (x) (detached-open-session (detached--decode-session x)))
    :items
    ,(lambda ()
       (pkg-detached/consult--filter-candidates #'detached-session-remotehost-p)))
  "Remote host `detached' sessions as a source for `consult'.")

(defvar pkg-detached/consult--source-current-session
  `(:narrow (?c . "Current Host")
    :hidden t
    :category detached
    :annotate detached-session-annotation
    :action (lambda (x) (detached-open-session (detached--decode-session x)))
    :items
    ,(lambda ()
       (let ((host-name (car (detached--host))))
        (pkg-detached/consult--filter-candidates
         (lambda (session)
           (string= (detached-session-host-name session) host-name))))))
  "Current host `detached' sessions as a source for `consult'.")

;;; Commands

(defun pkg-detached/delete-all-sessions ()
  (interactive)
  (let ((n-sessions (length detached--sessions)))
    (cond
     ((= n-sessions 1)
      (let ((session (seq-first (map-values detached--sessions))))
        (detached-kill-session session 'delete)))
     ((> n-sessions 0)
      (when (yes-or-no-p (format "Do you want to delete %s session(s)?" n-sessions))
        (call-interactively #'detached-delete-sessions))))))

(defvar pkg-detached/session-map
  (define-keymap
    "a" #'detached-edit-session-annotation
    "d" #'detached-detach-session
    "x" #'detached-delete-session
    "X" #'pkg-detached/delete-all-sessions
    "D" #'detached-describe-duration
    "e" #'detached-edit-and-run-session
    "k" #'detached-kill-session
    "o" #'detached-open-session
    "r" #'detached-rerun-session
    "S" #'detached-describe-session
    "v" #'detached-view-session
    "w" #'detached-copy-session-command
    "W" #'detached-copy-session-output))

(lib-util/pkg detached
  :elpaca (:ref "6b64d4d8064cee781e071e825857b442ea96c3d9")
  :disabled t
  :defer t
  :init
  (general-def
    :keymaps 'my/keys-mode-map
    :states '(normal insert emacs visual)
    :prefix my/leader
    :non-normal-prefix my/non-normal-prefix
    "o d" '(:keymap pkg-detached/session-map :package detached))

  (general-def
    [remap async-shell-command] #'detached-shell-command
    [remap compile] #'detached-compile
    [remap recompile] #'detached-compile-recompile
    [remap detached-open-session] #'detached-consult-session)

  (setq detached-db-directory (concat my/cache-dir "detached/"))
  (setq detached-session-directory "/tmp/detached")

  (setq detached-init-allow-list
        '(compile
          dired
          embark
          eshell
          org
          vterm))

  ;; Don't show notifications.
  (setq detached-notification-function #'ignore
        detached-state-transition-notifications-message #'ignore)

  ;; Number of context lines to display for a session.
  (setq detached-session-context-lines 50)

  (setq detached-open-session-display-buffer-action
        '(display-buffer-same-window))

  (setq detached-consult-sources
        '(detached-consult--source-session
          pkg-detached/consult--source-active-session
          pkg-detached/consult--source-inactive-session
          pkg-detached/consult--source-hidden-session
          pkg-detached/consult--source-success-session
          pkg-detached/consult--source-failure-session
          pkg-detached/consult--source-local-session
          pkg-detached/consult--source-remote-session
          pkg-detached/consult--source-current-session))

  :config
  (detached-init))

(provide 'pkg-detached)
