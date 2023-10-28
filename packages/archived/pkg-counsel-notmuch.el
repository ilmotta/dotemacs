;;; -*- lexical-binding: t; -*-
(require 'use-package)

;; Now that I'm not using counsel, some of the features in `counsel-notmuch'
;; should be ported. Ivy interface to search e-mails with notmuch-search(1).
(lib-util/pkg counsel-notmuch
  :disabled t
  :straight t
  :after (counsel notmuch)
  :demand t

  :general
  ("C-c m s" #'counsel-notmuch)
  ("C-c m i" #'my/counsel-notmuch-jump-to-inbox)
  ("C-c m t f" #'my/counsel-notmuch-find-tags)

  :config
  (defvar my/counsel-notmuch--find-tags-history nil
    "History for `my/counsel-notmuch-find-tags'.")

  (defvar my/counsel-notmuch--find-tags-command
    "notmuch search --exclude=false --format=text --output=tags -- '*'"
    "Shell command to find all available tags in the notmuch
      database.")

  (defun my/counsel-notmuch--find-tags-function (str)
    (setq ivy--old-re (ivy--regex-fuzzy str))
    (let ((cmd (concat my/counsel-notmuch--find-tags-command
                       " | grep --color=never '%s'")))
      (counsel--async-command (format cmd str))
      nil))

  (defun my/counsel-notmuch--find-tags-action (tag)
    (with-ivy-window
     (notmuch-search (format "tag:%s" tag))))

  (defun my/counsel-notmuch-jump-to-inbox ()
    (interactive)
    (notmuch-search "tag:inbox"))

  (defun my/counsel-notmuch-find-tags (&optional initial-input)
    (interactive)
    (ivy-read "Notmuch tag: "
              #'my/counsel-notmuch--find-tags-function
              :initial-input initial-input
              :re-builder #'ivy--regex-fuzzy
              :dynamic-collection t
              :history 'my/counsel-notmuch--find-tags-history
              :action #'my/counsel-notmuch--find-tags-action
              :unwind #'counsel-delete-process
              :caller 'my/counsel-notmuch-find-tags))

  (with-eval-after-load 'ivy
    (ivy-configure #'my/counsel-notmuch-find-tags
                   :exit-codes '(1 "Tag not found"))))

(provide 'pkg-counsel-notmuch)
