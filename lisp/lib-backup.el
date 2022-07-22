;;; -*- lexical-binding: t; -*-

(defun lib/rsync-extended-features ()
  "Return only available rsync options.
Some of these flags were added in the 3.2.3 release (Aug 2020)."
  (let ((features '())
        (options '("--atimes" "--crtimes" "--open-noatime")))
    (seq-doseq (opt options)
      (let ((output (shell-command-to-string (format "rsync --dry-run %s" opt))))
        (unless (or (string-match-p (format "rsync: %s: unknown option" opt) output)
                    (string-match-p (format "rsync does not support %s" opt) output))
          (push opt features))))
    features))

(defconst lib/mirror-cmd
  "rsync -vha --delete --stats \"%s\" \"%s\"")

(defun lib/rsync-extended-mirror-cmd ()
  (string-join `("rsync"
                 ;; "--dry-run"
                 "--verbose"
                 "--exclude" "node_modules"
                 ,(format "--log-file='%s'" (concat (temporary-file-directory) "rsync.log"))
                 "--archive"
                 ,@(lib/rsync-extended-features)
                 "--hard-links"
                 "--executability"
                 "--xattrs"
                 "--delete"
                 "--stats"
                 "--human-readable"
                 "--progress"
                 "\"%s\""
                 "\"%s\"")
               " "))

(defun lib/rsync-mirror-silent-cmd ()
  (string-join `("rsync"
                 "--archive"
                 ,@(lib/rsync-extended-features)
                 "--hard-links"
                 "--executability"
                 "--xattrs"
                 "--delete"
                 "\"%s\""
                 "\"%s\"")
               " "))

;;;###autoload
(defun lib/mirror-directories (src dst &optional force-p)
  "Mirror SRC to DST using rsync."
  (interactive
   (list (read-directory-name "Source directory: " default-directory nil nil)
         (read-directory-name "Destination directory: " default-directory nil nil)))
  (cl-assert (file-exists-p src))
  (cl-assert (file-directory-p src))
  (let* ((src (file-name-as-directory (expand-file-name src)))
         (dst (file-name-as-directory (expand-file-name dst)))
         (cmd (format (lib/rsync-extended-mirror-cmd) src dst))
         (buffer (get-buffer-create "*Sync*"))
         (do-run-sync (lambda ()
                        (make-directory dst 'parents)
                        (start-process-shell-command "sync" buffer cmd)
                        (display-buffer buffer))))
    (cond (force-p (funcall do-run-sync))
          ((file-exists-p dst)
           (when (yes-or-no-p (format "Destination directory \"%s\" will be replaced. Proceed?" dst))
             (funcall do-run-sync)))
          (:default (message "Skipped")))))

(comment
  ;; Documents
  (let* ((disk-mirror (concat "/run/media/" (user-login-name) "/MIRROR_3TB/"))
         (disk-sata1 (concat "/disks/sata1/"))
         (src (concat disk-sata1 "Documents/"))
         (dst (concat disk-mirror "Documents/")))
    (cl-assert (file-exists-p src))
    (cl-assert (file-exists-p dst))
    (cl-assert (file-directory-p src))
    (lib/mirror-directories src dst 'force))

  ;; Downloads
  (let* ((disk-mirror (concat "/run/media/" (user-login-name) "/MIRROR_3TB/"))
         (disk-sata1 (concat "/disks/sata1/"))
         (src (concat disk-sata1 "Downloads/"))
         (dst (concat disk-mirror "Downloads/")))
    (cl-assert (file-exists-p src))
    (cl-assert (file-exists-p dst))
    (cl-assert (file-directory-p src))
    (lib/mirror-directories src dst 'force))

  ;; Music
  (let* ((disk-mirror (concat "/run/media/" (user-login-name) "/MIRROR_3TB/"))
         (disk-sata1 (concat "/disks/sata1/"))
         (src (concat disk-sata1 "Music/"))
         (dst (concat disk-mirror "Music/")))
    (cl-assert (file-exists-p src))
    (cl-assert (file-exists-p dst))
    (cl-assert (file-directory-p src))
    (lib/mirror-directories src dst 'force))

  ;; Backups
  (let* ((disk-mirror (concat "/run/media/" (user-login-name) "/MIRROR_3TB/"))
         (disk-sata1 (concat "/disks/sata1/"))
         (src (concat disk-sata1 "Backups/"))
         (dst (concat disk-mirror "Backups/")))
    (cl-assert (file-exists-p src))
    (cl-assert (file-exists-p dst))
    (cl-assert (file-directory-p src))
    (lib/mirror-directories src dst 'force))

  ;; Videos
  (let* ((disk-mirror (concat "/run/media/" (user-login-name) "/MIRROR_3TB/"))
         (disk-sata1 (concat "/disks/sata1/"))
         (src (concat disk-sata1 "Videos/"))
         (dst (concat disk-mirror "Videos/")))
    (cl-assert (file-exists-p src))
    (cl-assert (file-exists-p dst))
    (cl-assert (file-directory-p src))
    (lib/mirror-directories src dst 'force))

  ;; Photos
  (let* ((disk-mirror (concat "/run/media/" (user-login-name) "/MIRROR_3TB/"))
         (disk-sata1 (concat "/disks/sata1/"))
         (src (concat disk-sata1 "Photos/"))
         (dst (concat disk-mirror "Photos/")))
    (cl-assert (file-exists-p src))
    (cl-assert (file-exists-p dst))
    (cl-assert (file-directory-p src))
    (lib/mirror-directories src dst 'force)))

(defun lib/-file-remote-p (path)
  (string-match-p (rx line-start
                      (one-or-more alphanumeric)
                      ":/")
                  path))

(defun lib/remote-mirror-directories-silent (src dst)
  "Mirror SRC to DST using rsync."
  (interactive)
  (let* ((src (if (lib/-file-remote-p src)
                  (seq-let [remote path] (split-string src ":")
                    (s-join ":" (list remote (file-name-as-directory path))))
                (file-name-as-directory (expand-file-name src))))
         (dst (if (lib/-file-remote-p dst)
                  (seq-let [remote path] (split-string dst ":")
                    (s-join ":" (list remote (file-name-as-directory path))))
                (file-name-as-directory (expand-file-name dst))))
         (cmd (format (lib/rsync-mirror-silent-cmd) src dst))
         (proc-name "sync-silent"))
    (unless (lib/-file-remote-p src)
      (cl-assert (file-exists-p src) 'show-args)
      (cl-assert (file-directory-p src) 'show-args))
    (if-let ((proc (get-process proc-name)))
        (message (propertize "Please, sync again after the current process finishes." 'face 'compilation-warning))
      (make-directory dst 'parents)
      (start-process-shell-command proc-name (get-buffer-create "*sync*") cmd))))

(provide 'lib-backup)

;; Local Variables:
;; read-symbol-shorthands: (("lib/" . "lib-backup/"))
;; End:
