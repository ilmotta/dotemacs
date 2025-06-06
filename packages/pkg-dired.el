;;; -*- lexical-binding: t; -*-

(require 'lib-util)

;;; Variables

(defvar-local pkg-dired/hidden-show-p nil)

;; Enable `dired-hide-details-mode' by default.
(defvar pkg-dired/global-hide-details-mode t)

(defun pkg-dired/setup-mode-h ()
  "Toggle `dired-hide-details-mode' based on global var."
  (if pkg-dired/global-hide-details-mode
      (dired-hide-details-mode +1)
    (dired-hide-details-mode -1)))

;;; Commands

(defun pkg-dired/hide-details-mode ()
  "Toggle `dired-hide-details-mode' globally."
  (interactive)
  (if (bound-and-true-p dired-hide-details-mode)
      (progn
        (setq pkg-dired/global-hide-details-mode nil)
        (dired-hide-details-mode -1))
    (dired-hide-details-mode +1)
    (setq pkg-dired/global-hide-details-mode t)))

(defun pkg-dired/toggle-hidden ()
  "Show/hide dot-files."
  (interactive)
  (when (equal major-mode 'dired-mode)
    (if pkg-dired/hidden-show-p
        ;; If currently showing.
        (progn
          (dired-mark-files-regexp "^\\\.")
          (dired-do-kill-lines))
      ;; Otherwise just revert to re-show.
      (progn (revert-buffer)
             (setq-local pkg-dired/hidden-show-p t)))))

(defun pkg-dired/duplicate-this-file ()
  "Duplicate file on this line."
  (interactive)
  (let* ((do-format (lambda (filename count)
                      (if-let ((extension (file-name-extension filename)))
                          (format "%s %02d.%s" (file-name-base filename) count extension)
                        (format "%s %02d" filename count))))
         (original  (dired-get-filename t))
         (count     1)
         (new       (funcall do-format original count)))
    (while (file-exists-p new)
      (setq count (1+ count)
            new   (funcall do-format original count)))
    (dired-copy-file original new nil))
  (revert-buffer))

;;;; Init

(lib-util/pkg dired
  :elpaca nil
  :hook (dired-mode-hook . pkg-dired/setup-mode-h)

  :init
  ;; Don't pass "--dired" flag to "ls".
  (setq dired-use-ls-dired nil)

  ;; Always copy/delete recursively.
  (setq dired-recursive-copies 'always
        dired-recursive-deletes 'top)

  (setq dired-compress-directory-default-suffix ".tar.gz"
        dired-compress-file-default-suffix ".gz")

  (setq dired-auto-revert-buffer t
        dired-do-revert-buffer t
        dired-dwim-target t  ; Suggest a target for moving/copying intelligently
        dired-hide-details-hide-symlink-targets nil)

  ;; In macOS --group-directories-first is not available. Install GNU ls via
  ;; coreutils package.
  (setq dired-listing-switches "-lAFh --group-directories-first")

  ;; If non-nil, kill the current buffer when selecting a new directory.
  (setq dired-kill-when-opening-new-dired-buffer t)

  ;; Disable the prompt about whether I want to kill the Dired buffer for a
  ;; deleted directory.
  (setq dired-clean-confirm-killing-deleted-buffers nil)

  ;; Don't display free space.
  (setq dired-free-space nil)

  (setq
   ;; Emulate the GNU platform.
   ls-lisp-emulation nil
   ls-lisp-dirs-first t

   ;; Do not use external ls program.
   ls-lisp-use-insert-directory-program nil)

  :config
  (transient-define-prefix pkg-dired/transient ()
    :transient-non-suffix #'transient--do-stay
    [[:description "UI"
      ("(" "Toggle details" dired-hide-details-mode :transient t)
      ("h" "Toggle hidden files" pkg-dired/toggle-hidden :transient t)
      ("U" "Remove all marks" dired-unmark-all-marks :transient t)
      ("m" "Mark file" dired-mark :transient t)
      ("u" "Unmark file" dired-unmark :transient t)]
     [:description "Permissions"
      ("G" "Change group" dired-do-chgrp)
      ("M" "Change mode" dired-do-chmod)
      ("O" "Change owner" dired-do-chown)]
     [:description "File system"
      ("+" "Create directory" dired-create-directory)
      ("C" "Copy" dired-do-copy)
      ("D" "Delete" dired-do-delete)
      ("R" "Rename" dired-do-rename)]]
    [:description "View"
     :pad-keys t
     ("v"     "Examine file in view mode" dired-view-file)
     ("M-RET" "Display file or directory in another window" dired-display-file)
     ("!"     "Run shell command on marked files" dired-do-shell-command)
     ("Z"     "Compress or uncompress marked files" dired-do-compress)
     ("; e"   "Encrypt marked files" epa-dired-do-encrypt)
     ("; d"   "Decrypt marked files" epa-dired-do-decrypt)]))

(provide 'pkg-dired)
