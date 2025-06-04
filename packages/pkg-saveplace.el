;;; -*- lexical-binding: t; -*-

(require 'lib-util)

(defun pkg-saveplace/reposition ()
  "Force windows to recenter current line (with saved position)."
  (run-with-timer
   0 nil
   (lambda (buf)
     (when (buffer-live-p buf)
       (dolist (win (get-buffer-window-list buf nil t))
         (with-selected-window win (recenter)))))
   (current-buffer)))

(lib-util/pkg compile
  :ensure nil
  :init
  (add-hook 'elpaca-after-init-hook #'save-place-mode)
  (add-hook 'find-file-hook 'pkg-saveplace/reposition t)

  (setq save-place-file (concat my/cache-dir "saveplace"))
  (setq save-place-limit 100)

  ;; Don't ask for the compilation command in the minibuffer. I often prefer to
  ;; set this variable in a directory locals file.
  (setq compilation-read-command nil))

(provide 'pkg-saveplace)
