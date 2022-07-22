;;; -*- lexical-binding: t; -*-

;; Increase GC's thresholds in order to reduce start-up time. They must be reset
;; to their original values (or even lower) in the `emacs-startup-hook'.
(setq gc-cons-threshold most-positive-fixnum)

;; Prevent package.el from modifying this file. You wish to set this variable,
;; you must do so in the early init file.
(setq package-enable-at-startup nil)

;; `file-name-handler-alist' is consulted on every `require', `load' and various
;; path/io functions. You get a minor speed up by nooping this.
(put 'file-name-handler-alist 'initial-value file-name-handler-alist)

(unless noninteractive
  (setq file-name-handler-alist nil))

(load (concat user-emacs-directory "core/globals.el") nil 'no-message)

;;; Frame settings

(defun theme/read ()
  (ignore-errors
    (mapcar (lambda (line)
               (pcase-let ((`(,key ,value) (split-string line "=")))
                 `(,(intern (string-trim key)) . ,(string-trim value))))
             (split-string (with-temp-buffer
                             (insert-file-contents-literally "~/.theme")
                             (buffer-string))
                           "\n" 'omit-nulls))))

(let* ((theme (theme/read))
       (name (and theme (intern (alist-get 'SYSTEM_THEME theme))))
       (bg-color (if theme (alist-get 'THEME_BG_COLOR theme) my/default-dark-bg)))
  (setq my/theme (or name 'doom-one))
  (setq default-frame-alist `((fullscreen . fullheight)
                              (menu-bar-lines . 0)
                              (tool-bar-lines . 0)
                              (left-fringe . 10)
                              (horizontal-scroll-bars . nil)
                              (vertical-scroll-bars . nil)
                              (font . ,my/face-fixed-pitch-family)
                              (background-color . ,bg-color))))

;; Resizing the Emacs frame can be a terribly expensive part of changing the
;; font. By inhibiting this, we easily halve startup times with fonts that are
;; larger than the system default.
(setq frame-inhibit-implied-resize t)

;; Change font size of header lines too.
(setq-default text-scale-remap-header-line t)

;; Set initial frame settings to avoid rendering artifacts (e.g. seeing the menu
;; bar before it's effectively disabled).
(setq initial-frame-alist '((fullscreen . maximized)))
