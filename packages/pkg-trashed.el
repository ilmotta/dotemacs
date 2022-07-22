;;; -*- lexical-binding: t; -*-

;;; Commentary:
;; Manage the system trash just like any other dired buffer.
;; `delete-by-moving-to-trash' should be set to t if you want to move files to
;; the trash when deleting from dired buffers.

;;; Code:

(my/package trashed
  :straight t
  :defer t
  :init
  (general-def
    :keymaps 'my/keys-mode-map
    :states '(normal insert visual emacs)
    :prefix my/leader
    :non-normal-prefix my/non-normal-prefix
    "o t" #'trashed)

  (setq trashed-action-confirmer #'y-or-n-p
        trashed-use-header-line t
        trashed-sort-key '("Date deleted" . t)
        trashed-date-format "%Y-%m-%d %H:%M:%S"))

(provide 'pkg-trashed)
