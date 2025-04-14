;;; -*- lexical-binding: t; -*-

;;; Code:
(defgroup my nil
  "My own global group."
  :group 'my
  :prefix "my/")

;;; Constants

(defun my/os ()
  (let ((uname (shell-command-to-string "uname -a"))
        (android-regexp (rx "Linux" (zero-or-more not-newline) "Android"))
        (surface-regexp (rx "Linux" (zero-or-more not-newline) "surface")))
    (cond ((string-match-p android-regexp uname) 'system/android)
          ((string-match-p surface-regexp uname) 'system/linux/surface)
          ((equal system-type 'windows-nt) 'system/windows)
          ((equal system-type 'darwin) 'system/macos)
          (:default 'system/linux))))

(defconst my/system-type (my/os))
(defconst my/android? (equal my/system-type 'system/android))
(defconst my/mac?   (equal my/system-type 'system/macos))
(defconst my/windows? (equal my/system-type 'system/windows))
(defconst my/surface? (equal my/system-type 'system/linux/surface))
(defconst my/machine-matrix-p (equal "matrix" (system-name)))
(defconst my/linux? (or (equal my/system-type 'system/linux)
                        (equal my/system-type 'system/linux/surface)))

(defconst my/lisp-modes
  '(cider-repl-mode
    clojure-mode
    clojurec-mode
    clojurescript-mode
    clojurex-mode
    common-lisp-mode
    emacs-lisp-mode
    geiser-repl-mode
    inf-clojure-mode
    inferior-emacs-lisp-mode
    inferior-lisp-mode
    inferior-scheme-mode
    lisp-data-mode
    lisp-interaction-mode
    lisp-mode
    scheme-interaction-mode
    scheme-mode
    slime-repl-mode))

(defconst my/lisp-modes-maps
  (mapcar (lambda (mode)
            (intern (concat (symbol-name mode) "-map")))
          my/lisp-modes))

(defconst my/lisp-modes-hooks
  (mapcar (lambda (mode)
            (intern (concat (symbol-name mode) "-hook")))
          my/lisp-modes))

;;; Variables

(defvar my/default-file-name-handler-alist nil
  "Hold the original value to be restored after initialization.")

(defvar my/start-up-benchmarks nil
  "Store start-up times of every loaded package.")

(defvar my/start-up-time nil
  "The total time it took to start-up Emacs (in milliseconds).")

(defvar-local my/real-buffer-p nil
  "If non-nil, this buffer should be considered real no matter
what. See `lib-util/real-buffer-p' for more information.")

;; Consistently load theme when running Emacs as a daemon (see discussion in
;; https://stackoverflow.com/questions/18904529/after-emacs-deamon-i-can-not-see-new-theme-in-emacsclient-frame-it-works-fr).
(defvar my/theme-window-loaded nil)
(defvar my/theme-terminal-loaded nil)

;;; Custom

(defcustom my/evil-p t
  "When non-nil, enable evil and all related dependencies."
  :type 'boolean)

(defcustom my/windmove-modifier "M"
  "Either Meta (M) or Super (s)."
  :type 'string)

(defcustom my/reuse-buffer-window-p t
  "Non-nil enables various advice functions so buffer
switching/popping operations try to reuse the existing buffer
window by binding `display-buffer-overriding-action'.

This setting only has effect if
`switch-to-buffer-obey-display-actions' is true."
  :type 'boolean)

(defcustom my/outline-regex-lisp
  "[ \t]*;;;;* [^ \t\n]"
  "Regular expression to match the beginning of a heading."
  :type 'string)

(defcustom my/use-package-force-demand t
  "Non-nil forces use-package to set :demand t."
  :type 'boolean)

(defcustom my/emacs-dir (eval-when-compile (file-truename user-emacs-directory))
  "The path to the currently loaded .emacs.d directory. Must end
  with a slash."
  :type 'string)

(defcustom my/local-dir (concat my/emacs-dir ".local/")
  "Root directory for local storage. Must end with a slash."
  :type 'string)

(defcustom my/cache-dir (concat my/local-dir "cache/")
  "Directory for volatile local storage. Must end with a slash."
  :type 'string)

(defcustom my/non-normal-prefix "s-SPC"
  "Non-normal prefix key used exclusively for keybindings related to the
current major mode."
  :type 'string)

(defcustom my/leader "SPC"
  "Global prefix used in `general' keybindings."
  :type 'string)

(defcustom my/local-leader "m"
  "Local leader prefix."
  :type 'string)

(defcustom my/consult-preview-and-next-line-key "M-J"
  "Key used by `consult-preview-key' and by `next-line' in the
minibuffer."
  :type 'string)

(defcustom my/consult-preview-and-previous-line-key "M-K"
  "Key used by `consult-preview-key' and by `previous-line' in the
minibuffer."
  :type 'string)

(defcustom my/default-gc-cons-threshold (* 16 1024 1024)
  "The default portion of the heap used for allocation (16 MB)."
  :type 'integer)

(defcustom my/show-start-up-time? t
  "Non-nil will print the total time to start-up."
  :type 'boolean)

(defcustom my/escape-hook nil
  "Hook for canceling operations in normal mode."
  :type 'hook)

(defcustom my/gtk-dark-theme "Adwaita-dark"
  "Default GTK dark theme name."
  :type 'string)

(defcustom my/gtk-light-theme "Adwaita"
  "Default GTK light theme name."
  :type 'string)

(defcustom my/kde-dark-theme "org.kde.breezedark.desktop"
  "Default KDE Plasma dark theme name."
  :type 'string)

(defcustom my/kde-light-theme "org.kde.breeze.desktop"
  "Default KDE Plasma light theme name."
  :type 'string)

(defcustom my/favorite-themes '(zenburn doom-dark+ doom-one doom-monokai-pro spacemacs-light doom-one-light)
  "My favorite themes."
  :type '(list symbol))

(defcustom my/favorite-dark-theme 'doom-one
  "My favorite dark theme for quick switching."
  :type 'symbol)

(defcustom my/favorite-light-theme 'doom-one-light
  "My favorite light theme for quick switching."
  :type 'symbol)

(defcustom my/theme 'doom-one
  "Current theme."
  :type 'symbol)

(defcustom my/face-variable-pitch-family "Bookerly"
  "Default variable pitch font family."
  :type 'string
  :group 'my)

(defcustom my/face-fixed-pitch-family (if my/mac?
                                          "JetBrains Mono:weight=light:height=14"
                                        "JetBrainsMono:weight=light:height=14")
  "Default fixed pitch font family."
  :type 'string)

(defcustom my/default-dark-bg "#282C34"
  "Default frame dark background."
  :type 'string)

(defcustom my/face-org-headline-family my/face-variable-pitch-family
  "Default Org headline font family."
  :type 'string)

(defcustom my/note-id-time-string "%Y%m%d%H%M%S"
  "Format used to name new note files."
  :type 'string)

;;; Modes

(define-minor-mode my/keys-mode
  "Personal keybindings."
  :init-value t
  :lighter " MK"
  :global t
  :keymap '())

(provide 'my-globals)
