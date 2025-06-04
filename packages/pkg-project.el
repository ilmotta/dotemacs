;;; -*- lexical-binding: t; -*-

(require 'lib-util)

(defun pkg-project/locate-project (dir)
  (let ((override (locate-dominating-file dir ".project.el")))
    (if override
        (cons 'vc override)
      nil)))

(defun pkg-project/kill-buffers ()
  "Kill buffers belonging to the current project.
Except current buffer."
  (interactive)
  (let* ((window-buf (current-buffer))
         (kill-buffer-query-functions (cons (lambda ()
                                              (not (equal (current-buffer) window-buf)))
                                            kill-buffer-query-functions)))
    (project-kill-buffers)))

(defun pkg-project/close-tab-and-kill-buffers ()
  "Kill all buffers and close current tab."
  (interactive)
  (let ((window-buf (current-buffer))
        (kill-buffer-query-functions nil))
    (project-kill-buffers)
    (when (> (length (funcall tab-bar-tabs-function)) 1)
      (tab-close))))

(lib-util/pkg project
  :ensure nil
  :init
  (general-def
    [remap project-kill-buffers] #'pkg-project/kill-buffers)

  (add-hook 'project-find-functions #'project-try-vc)
  (add-hook 'project-find-functions #'pkg-project/locate-project)

  (setq project-list-file (concat my/cache-dir "projects"))
  (setq project-switch-commands #'project-dired))

(provide 'pkg-project)
