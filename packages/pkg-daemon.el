;;; -*- lexical-binding: t; -*-

(defun pkg-daemon/after-make-frame-functions-h (frame)
  (when (daemonp)
    (select-frame frame)
    (if (window-system frame)
        (unless my/theme-window-loaded
          (if my/theme-terminal-loaded
              (enable-theme my/theme)
            (load-theme my/theme 'no-confirm))
          (setq my/theme-window-loaded t))
      (unless my/theme-terminal-loaded
        (if my/theme-window-loaded
            (enable-theme my/theme)
          (load-theme my/theme 'no-confirm))
        (setq my/theme-terminal-loaded t)))))

(defun pkg-daemon/inhibit-emacsclient-message ()
  "Inhibit initial emacsclient frame message.

Remove initial message 'When done with this frame type C-x 5 0'
when starting with 'emacsclient --create-frame' (the original
solution is in
https://emacs.stackexchange.com/questions/44883/remove-emacsclient-startup-message)."
  (setq inhibit-message t)
  (run-with-idle-timer 0 nil (lambda () (setq inhibit-message nil))))

(add-hook 'server-after-make-frame-hook #'pkg-daemon/inhibit-emacsclient-message)
(add-hook 'after-make-frame-functions #'pkg-daemon/after-make-frame-functions-h)

(provide 'pkg-daemon)
