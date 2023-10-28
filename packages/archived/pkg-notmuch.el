;;; -*- lexical-binding: t; -*-
(require 'use-package)

(defun pkg-notmuch/move-cursor-to-first-search-button ()
  (if (and (eq (point) (point-min))
           (search-forward "Saved searches:" nil t))
      (progn
        (forward-line)
        (widget-forward 1))
    (if (eq (widget-type (widget-at)) 'editable-field)
        (beginning-of-line))))

(defun pkg-notmuch/notmuch-hello ()
  "Load `notmuch' before calling `notmuch-hello'."
  (interactive)
  (require 'notmuch)
  (notmuch-hello))

;; Major mode for interacting with Notmuch (an email tag system).
(unless my/windows?
  (lib-util/pkg notmuch
    :straight t
    :defer t

    :hook (mail-mode-hook . flyspell-mode)
    :hook (notmuch-hello-refresh-hook . pkg-notmuch/move-cursor-to-first-search-button)

    :init
    (general-def
      :keymaps 'notmuch-common-keymap
      :states 'normal
      "m h" #'pkg-notmuch/notmuch-hello
      "m t j" #'notmuch-jump-search)

    :config
    ;; Show newest first.
    (setq notmuch-search-oldest-first nil)

    ;; Tags starting with "+" (or not starting with either "+" or "-") in
    ;; the list will be added, and tags starting with "-" will be removed
    ;; from the message or thread being archived.
    (setq notmuch-archive-tags '("-inbox" "+archived"))

    (setq notmuch-saved-searches
          '((:name "inbox" :query "tag:inbox" :key "i")
            (:name "unread" :query "tag:unread" :key "u")
            (:name "flagged" :query "tag:flagged" :key "f")
            (:name "sent" :query "tag:sent" :key "t")
            (:name "drafts" :query "tag:draft" :key "d")
            (:name "trash" :query "tag:/.*/ and folder:\"/.*gmail-trash/\"" :key "T")
            (:name "spam" :query "tag:/.*/ and folder:\"/.*gmail-spam/\"" :key "S")
            (:name "all mail" :query "tag:/.*/ AND NOT (tag:spam OR tag:deleted)" :key "a")))))

(provide 'pkg-notmuch)
