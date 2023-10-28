;;; -*- lexical-binding: t; -*-

;;; Code:

;;;###autoload
(defun lib-fs/symlinks-to-file ()
  (interactive)
  (when-let ((file (buffer-file-name)))
    (let ((cmd (format "symlinks -rv ~/ | grep '%s'" file)))
      (promise-chain (lib-sys/promise-start-process-shell-command cmd)
        (then (lambda (out)
                (message "%s" out)))
        (catch (lambda (err)
                 (message "Error '%s'" err)))))))

;;;###autoload
(defun lib-fs/rename-file-and-buffer ()
  "Rename current buffer and if the buffer is visiting a file,
rename it too."
  (interactive)
  (let ((filename (buffer-file-name)))
    (if (not (and filename (file-exists-p filename)))
        (rename-buffer (read-from-minibuffer "New name: " (buffer-name)))
      (let* ((new-name (read-file-name "New name: " (file-name-directory filename)))
             (containing-dir (file-name-directory new-name)))
        (make-directory containing-dir t)
        (cond
         ((vc-backend filename) (vc-rename-file filename new-name))
         (t
          (rename-file filename new-name t)
          (set-visited-file-name new-name t t)))))))

(provide 'lib-fs)
