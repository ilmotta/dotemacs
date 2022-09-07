;;; -*- lexical-binding: t; -*-

(defun pkg-saveplace/reposition ()
  "Force windows to recenter current line (with saved position)."
  (run-with-timer
   0 nil
   (lambda (buf)
     (when (buffer-live-p buf)
       (dolist (win (get-buffer-window-list buf nil t))
         (with-selected-window win (recenter)))))
   (current-buffer)))

(my/package saveplace
  :straight (:type built-in)
  :defer t

  :hook (after-init-hook . save-place-mode)

  :init
  (setq save-place-file (concat my/cache-dir "saveplace"))
  (setq save-place-limit 100)

  :config
  (add-hook 'find-file-hook 'pkg-saveplace/reposition t))

(provide 'pkg-saveplace)
