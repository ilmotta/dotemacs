;;; -*- lexical-binding: t; -*-

(require 'lib-util)

(defun pkg-markdown-mode/export-as-plain-text ()
  "Replaces region with plain text version using pandoc."
  (interactive)
  (cl-assert (executable-find "pandoc") nil "Program 'pandoc' not found")
  (cl-assert (use-region-p) nil "No region selected")
  (let* ((src-buf (current-buffer))
         (reg-beginning (region-beginning))
         (reg-end (region-end))
         (buf (generate-new-buffer " *md-export-as-plain-text*"))
         (on-success (lambda ()
                       (let ((plain-text (with-current-buffer buf
                                           (buffer-string))))
                         (kill-buffer buf)
                         (with-current-buffer src-buf
                           (delete-region reg-beginning reg-end)
                           (insert plain-text)))))
         (proc (make-process
                :name "md-export-as-plain-text"
                :buffer buf
                :command (list (executable-find "pandoc")
                               "--from" "markdown"
                               "--to" "plain")
                :connection-type 'pipe
                :sentinel (lambda (proc state)
                            (if (zerop (process-exit-status proc))
                                (funcall on-success)
                              (message "Failed to export to plain text. Error '%s'\n%s"
                                       state
                                       (with-current-buffer buf
                                         (buffer-string))))))))
    (process-send-region proc reg-beginning reg-end)
    (process-send-eof proc)))

(lib-util/pkg markdown-mode
  :elpaca (:ref "d95107f5b77d6c010e89259e05adfcd79a21f26a")
  :defer t
  :mode (("\\.md\\'" . markdown-mode)
         ("\\.markdown\\'" . markdown-mode)))

(provide 'pkg-markdown-mode)
