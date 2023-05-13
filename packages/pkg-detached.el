;;; -*- lexical-binding: t; -*-

;;; Commentary:
;; Archived on 2023-05-13 because it throws an error while calling
;; `detached-init':
;;
;;     Wrong type argument: listp, \...

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
      (call-interactively #'detached-delete-sessions))
     ((> n-sessions 0)
      (when (yes-or-no-p (format "Do you want to delete %s session(s)?" n-sessions))
        (call-interactively #'detached-delete-sessions))))))

(defvar pkg-detached/session-map
  (let ((map (make-sparse-keymap)))
    (define-key map "a" #'detached-edit-session-annotation)
    (define-key map "d" #'detached-detach-session)
    (define-key map "x" #'detached-delete-session)
    (define-key map "X" #'pkg-detached/delete-all-sessions)
    (define-key map "D" #'detached-describe-duration)
    (define-key map "e" #'detached-edit-and-run-session)
    (define-key map "k" #'detached-kill-session)
    (define-key map "o" #'detached-open-session)
    (define-key map "r" #'detached-rerun-session)
    (define-key map "S" #'detached-describe-session)
    (define-key map "v" #'detached-view-session)
    (define-key map "w" #'detached-copy-session-command)
    (define-key map "W" #'detached-copy-session-output)
    map))

(my/package
  (detached :ref "6b64d4d8064cee781e071e825857b442ea96c3d9")
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
