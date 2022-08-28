;;; -*- lexical-binding: t; -*-

(defun pkg-flycheck/list-errors ()
  (interactive)
  (lib-util/toggle-buffer flycheck-error-list-buffer #'flycheck-list-errors))

(defun my/use-eslint-from-node-modules ()
  "Use local eslint from node_modules before global.
  http://emacs.stackexchange.com/questions/21205/flycheck-with-file-relative-eslint-executable"
  (let* ((root (locate-dominating-file
                (or (buffer-file-name) default-directory)
                "node_modules"))
         (eslint (and root (expand-file-name "node_modules/eslint/bin/eslint.js"
                                             root))))
    (when (and eslint (file-executable-p eslint))
      (setq-local flycheck-javascript-eslint-executable eslint))))

(my/package flycheck
  :straight t
  :defer t

  :hook (prog-mode-hook . flycheck-mode)
  :hook (flycheck-mode-hook . my/use-eslint-from-node-modules)

  :init
  (general-def
    :keymaps 'flycheck-command-map
    "l" #'pkg-flycheck/list-errors)

  ;; When set to ‘inherit’, use the ‘load-path’ of the current Emacs
  ;; session during syntax checking.  This is an important
  ;; configuration to avoid flycheck errors when requiring libraries.
  (setq flycheck-emacs-lisp-load-path 'inherit)

  ;; Flycheck should create hidden temporary files.
  (setq flycheck-temp-prefix ".flycheck")

  (setq flycheck-keymap-prefix (kbd "C-c !"))

  ;; Reduce syntax checking frequency.
  (setq flycheck-idle-change-delay 0.25)

  ;; Display errors a little quicker (default is 0.9s).
  (setq flycheck-display-errors-delay 0.25)

  ;; If set to nil, do not indicate errors and warnings, but just highlight them
  ;; according to `flycheck-highlighting-mode'.
  (setq flycheck-indication-mode nil)

  ;; Do not highlight errors at all. However, errors will still be reported in
  ;; the mode line and in error message popups,and indicated according to
  ;; flycheck-indication-mode.
  (setq flycheck-highlighting-mode 'symbols)

  :config
  (add-to-list 'display-buffer-alist
               `(,(rx (literal flycheck-error-list-buffer))
                 (display-buffer-in-side-window)
                 (window-height . 0.33)
                 (side . bottom)
                 (slot . 0)
                 (body-function . select-window)))

  (delq 'new-line flycheck-check-syntax-automatically)

  ;; Flycheck slow when opening JS files while waiting for eslint.
  ;; https://github.com/flycheck/flycheck/issues/1129
  (advice-add 'flycheck-eslint-config-exists-p :override (lambda() t)))

(provide 'pkg-flycheck)
