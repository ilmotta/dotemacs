;;; -*- lexical-binding: t; -*-

(defun pkg-recentf/add-dired-directory ()
  "Add dired directory to recentf file list."
  (recentf-add-file default-directory))

(defun pkg-recentf/file-truename (file)
  (if (or (file-remote-p file nil t)
          (not (file-remote-p file)))
      (file-truename file)
    file))

(my/package recentf
  :straight (:type built-in)
  :defer t

  :hook (kill-emacs-hook . recentf-cleanup)
  :hook (dired-mode-hook . pkg-recentf/add-dired-directory)
  :hook (after-init-hook . recentf-mode)

  :init
  (setq recentf-save-file (concat my/cache-dir "recentf")
        recentf-auto-cleanup 'never
        recentf-max-menu-items 0
        recentf-max-saved-items 100)

  ;; Functions to post process recent file names. They are successively passed a
  ;; file name to transform it.
  (setq recentf-filename-handlers
        '(substring-no-properties
          pkg-recentf/file-truename
          abbreviate-file-name)))

(provide 'pkg-recentf)
