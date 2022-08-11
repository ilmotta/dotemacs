;;; -*- lexical-binding: t; -*-

;;; Commentary:
;;
;; Sensible defaults that don't depend on external packages.

;;; Code:
;; Prevent Custom from modifying the init file.
(let ((file (expand-file-name ".local/cache/custom.el" user-emacs-directory)))
  (make-directory (file-name-directory file) 'parents)
  (setq custom-file file)
  (load custom-file 'noerror 'nomessage))

;;; Noise reduction

;; `file-name-handler-alist' is consulted on every `require', `load' and various
;; path/io functions. You get a minor speed up by nooping this.
(defvar my/default-file-name-handler-alist file-name-handler-alist)
(unless noninteractive
  (setq file-name-handler-alist nil))

(defun my/mute-load (original-fn file &optional noerror _nomessage nosuffix must-suffix)
  (funcall original-fn file noerror 'nomessage nosuffix must-suffix))

(defun my/unmute-load ()
  (advice-remove 'load #'my/mute-load))

;; This advice is used mainly to mute messages from the
;; `guix-emacs-autoload-packages' function. The advice is then removed after
;; (hopefully) all the noisy stuff has been loaded.
(advice-add 'load :around #'my/mute-load)

;; Bring the `load' function behavior back to normal after (hopefully) all the
;; noisy stuff has been loaded.
(add-hook 'after-init-hook #'my/unmute-load)

;;; Runtime optimizations

(defun my/emacs-startup-h ()
  "Restore default values after startup."
  ;; Restore `file-name-handler-alist', because it is needed for handling
  ;; encrypted or compressed files, among other things.
  (setq file-name-handler-alist (get 'file-name-handler-alist 'initial-value))

  ;; Reset GC's thresholds to normal values.
  (setq gc-cons-threshold my/default-gc-cons-threshold)

  (when my/show-start-up-time?
    (setq my/start-up-time (* 1000 (float-time (time-subtract after-init-time before-init-time))))
    (let ((inhibit-message t))
      (message "Loaded in %dms." my/start-up-time))))

(add-hook 'emacs-startup-hook #'my/emacs-startup-h)

;; In noninteractive sessions, prioritize non-byte-compiled source files to
;; prevent the use of stale byte-code. Otherwise, it saves us a little IO time
;; to skip the mtime checks on every *.elc file.
(setq load-prefer-newer noninteractive)

;; Emacs "updates" its ui more often than it needs to, so we slow it down
;; slightly, from 0.5s:
(setq idle-update-delay 1)

;; Don't make a second case-insensitive pass over `auto-mode-alist'. If it has
;; to, it's our (the user's) failure. One case for all!
(setq auto-mode-case-fold nil)

;; Disable bidirectional text rendering for a modest performance boost. Of
;; course, this renders Emacs unable to detect/display right-to-left languages.
(setq-default bidi-display-reordering 'left-to-right
              bidi-paragraph-direction 'left-to-right
              bidi-inhibit-bpa t)

;; Display the bare minimum at startup. We don't need all that noise. The
;; dashboard/empty scratch buffer is good enough.
(setq inhibit-startup-message t
      inhibit-splash-screen t
      inhibit-startup-echo-area-message user-login-name
      inhibit-default-init t
      initial-major-mode 'fundamental-mode
      initial-scratch-message nil)
(fset #'display-startup-echo-area-message #'ignore)

;;;###autoload
(defun my/buffer-new (&optional force-new-p)
  "Create new empty buffer. New buffer will be named *untitled*,
*untitled*<2>, etc."
  (interactive "P")
  (let ((buf-name (if force-new-p (generate-new-buffer "*untitled*")
                    "*untitled*")))
    (switch-to-buffer buf-name)
    (funcall initial-major-mode)
    (with-current-buffer buf-name
      (setq-local buffer-offer-save t))
    (get-buffer buf-name)))

;; Switch to the buffer returned by `lib-util/buffer-new'.
(setq initial-buffer-choice #'my/buffer-new)

;; Don't even leak the scratch buffer.
(ignore-errors (kill-buffer "*scratch*"))

;; Reduce rendering/line scan work for Emacs by not rendering cursors or regions
;; in non-focused windows.
(setq-default cursor-in-non-selected-windows nil)
(setq highlight-nonselected-windows nil)

;; Don't ping things that look like domain names.
(setq ffap-machine-p-known 'reject)

;; Don't resize windows & frames in steps; it's prohibitive to prevent the user
;; from resizing it to exact dimensions, and looks weird.
(setq window-resize-pixelwise t
      frame-resize-pixelwise t)

;; Remove command line options that aren't relevant to our current OS; means
;; slightly less to process at startup.
(unless my/mac?   (setq command-line-ns-option-alist nil))
(unless my/linux? (setq command-line-x-option-alist nil))

;;;; Scrolling

;; More performant rapid scrolling over unfontified regions. May cause brief
;; spells of inaccurate fontification immediately after scrolling.
(setq fast-but-imprecise-scrolling t)

;; Use character scroll.
(pixel-scroll-mode -1)

(setq hscroll-margin 2
      hscroll-step 1
      ;; Emacs spends too much effort recentering the screen if you scroll the
      ;; cursor more than N lines past window edges (where N is the settings of
      ;; `scroll-conservatively'). This is especially slow in larger files
      ;; during large-scale scrolling commands. If kept over 100, the window is
      ;; never automatically recentered.
      scroll-conservatively 101
      scroll-margin 0
      scroll-step 1
      scroll-preserve-screen-position t

      ;; Reduce cursor lag by a tiny bit by not auto-adjusting `window-vscroll'
      ;; for tall lines.
      auto-window-vscroll nil)

;; Mouse scroll
(when (version< emacs-version "29.0.50")
  (setq ;; Reduce scroll amount to a reasonable value.
   mouse-wheel-scroll-amount '(2 ((shift) . hscroll))
   mouse-wheel-scroll-amount-horizontal 2
   ;; Don't accelerate scrolling.
   mouse-wheel-progressive-speed nil))

;; Emacs 29: Experimental pixel scroll (compiled with --xinput2).
(when (version= "29.0.50" emacs-version)
  (setq pixel-scroll-precision-interpolate-page t
        pixel-scroll-precision-initial-velocity-factor (/ 0.0335 4)
        pixel-scroll-precision-interpolation-between-scroll 0.001
        pixel-scroll-precision-interpolation-factor 3.0
        pixel-scroll-precision-interpolation-total-time 0.1
        pixel-scroll-precision-large-scroll-height 30.0
        pixel-scroll-precision-momentum-min-velocity 10.0
        pixel-scroll-precision-momentum-seconds 1.75
        pixel-scroll-precision-momentum-tick 0.01
        pixel-scroll-precision-use-momentum nil)
  (pixel-scroll-precision-mode +1))

;;;; Garbage collector

;; Uncomment to watch the `gc-cons-threshold' variable.
;; (defun my/gc-cons-threshold-watcher (symbol newval operation where)
;;   (message "GC threshold: %s" newval))
;; (add-variable-watcher 'gc-cons-threshold my/gc-cons-threshold-watcher)

(defun my/increase-gc-threshold (&rest _args)
  "Increase GC threshold up to 128MB."
  (setq gc-cons-threshold (* 128 1024 1024)))

(defun my/reset-gc-threshold (&rest _args)
  (setq gc-cons-threshold my/default-gc-cons-threshold))

;; Temporarily increase the garbage collector threshold when the minibuffer is
;; active. This workaround pretty much eliminates stuttering when fast scrolling
;; over completion candidates. The same technique can be used in other places,
;; such as when `company-mode' completion is active.
(add-hook 'minibuffer-setup-hook #'my/increase-gc-threshold)
(add-hook 'minibuffer-exit-hook #'my/reset-gc-threshold)

(defun my/clean-gc ()
  (unless (frame-focus-state)
    (garbage-collect-maybe 10)))

(add-function :after after-focus-change-function #'my/clean-gc)

;;; Images

;; Animate image loops forever, rather than playing once.
(setq image-animate-loop t)

;; When you get to the right edge, it goes back to how it normally prints.
(setq prettify-symbols-unprettify-at-point 'right-edge)

;; Automatically enable `view-mode' because most of the time I never want to
;; modify read-only files and I want Emacs to properly warn me if I do so.
(setq view-read-only t)

;; Emacs 28: Hide commands in M-x which do not apply to the current mode.
(setq read-extended-command-predicate
      #'command-completion-default-include-p)

;;; Keybindings

(global-set-key [remap kill-buffer] #'kill-this-buffer)
(define-key my/keys-mode-map (kbd "M-;") #'lib-util/comment-dwim)
(define-key my/keys-mode-map (kbd "C-h K") #'describe-keymap)
(define-key my/keys-mode-map (kbd "C-h F") #'describe-face)

(define-key my/keys-mode-map (kbd "C-c 0") #'lib-system/main-t)
(define-key my/keys-mode-map (kbd "M-Q") #'lib-util/unfill-dwim)
(define-key my/keys-mode-map (kbd "C-c e P s") #'lib-util/profiler-start)
(define-key my/keys-mode-map (kbd "C-c e P k") #'lib-util/profiler-stop)

(when (version= "29.0.50" emacs-version)
  (define-key my/keys-mode-map (kbd "C-x <f5>") #'restart-emacs))

(defvar pkg-emacs/file-command-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "%")   #'query-replace-regexp)
    (define-key map (kbd "b")   #'consult-buffer)
    (define-key map (kbd "4 b") #'consult-buffer-other-window)
    (define-key map (kbd "d")   #'dired-jump)
    (define-key map (kbd "4 d") #'dired-jump-other-window)
    (define-key map (kbd "f")   #'find-file)
    (define-key map (kbd "l")   #'find-library)
    (define-key map (kbd "4 l") #'find-library-other-window)
    (define-key map (kbd "5 l") #'find-library-other-frame)
    (define-key map (kbd "n")   #'switch-to-next-buffer)
    (define-key map (kbd "p")   #'switch-to-prev-buffer)
    (define-key map (kbd "i")   #'ibuffer)
    (define-key map (kbd "k")   #'kill-buffer)
    (define-key map (kbd "K")   #'kill-buffer-and-window)
    (define-key map (kbd "c")   #'lib-util/buffer-new)
    (define-key map (kbd "C")   #'lib-util/clone-buffer-dwim)
    (define-key map (kbd "4 C") #'clone-indirect-buffer-other-window)
    (define-key map (kbd "o i") #'lib-util/find-user-init-file)
    (define-key map (kbd "o c") #'lib-util/find-user-custom-file)
    (define-key map (kbd "r")   #'revert-buffer-quick)
    (define-key map (kbd "R")   #'revert-buffer)
    (define-key map (kbd "s")   #'save-buffer)
    (define-key map (kbd "u f") #'lib-util/sudo-find-file)
    (define-key map (kbd "w")   #'write-file)
    (define-key map (kbd "y d") #'lib-util/yank-buffer-absolute-dir-path)
    (define-key map (kbd "y f") #'lib-util/yank-buffer-filename)
    (define-key map (kbd "y y") #'lib-util/yank-buffer-absolute-path)
    (define-key map (kbd "z")   #'bury-buffer)
    map))
(fset 'pkg-emacs/file-command-map pkg-emacs/file-command-map)

(define-key my/keys-mode-map (kbd "C-x K") #'kill-buffer-and-window)

;;; Long files

;;;; Init

(setq so-long-threshold 10000)

;; The new predicate efficiently knows which line is the longest in the
;; buffer, so it doesn't rely on `so-long-max-lines'.
(setq so-long-predicate #'so-long-statistics-excessive-p)

;;;; Config

(add-hook 'after-init-hook #'global-so-long-mode)

;;; Minibuffer

(defun pkg-minibuffer/truncate-lines ()
  (setq-local truncate-lines t))

(defun pkg-minibuffer/change-cursor-type ()
  "Change cursor type to a thin bar, like insert mode.

  By default the minibuffer uses a box cursor even with
  `evil-mode' enabled, but I prefer a bar to show that I'm in the
  insert state mode."
  (setq-local cursor-type '(bar . 2))
  (setq-local line-spacing 1))

(setq completion-category-defaults nil)
(setq completion-cycle-threshold nil)
(setq completion-flex-nospace nil)
(setq completion-pcm-complete-word-inserts-delimiters t)
(setq completion-pcm-word-delimiters "-_./:| ")
(setq completion-show-help nil)

;; Ignore case.
(setq completion-ignore-case t)
(setq read-buffer-completion-ignore-case t)
(setq read-file-name-completion-ignore-case t)
(setq-default case-fold-search t)

;; The following two are updated in Emacs 28.  They concern the
;; *Completions* buffer.
(setq completions-format 'one-column)
(setq completions-detailed t)

;; Allow for minibuffer-ception. Sometimes we need another minibuffer command
;; while we're in the minibuffer.
(setq enable-recursive-minibuffers t)

;; Accept short (single key-press) answers to the question
(setq read-answer-short t)

;; Let mini-windows grow only; they return to their normal size when the
;; minibuffer is closed, or the echo area becomes empty.
(setq resize-mini-windows 'grow-only)

;; Try really hard to keep the cursor from getting stuck in the
;; read-only prompt portion of the minibuffer.
(setq minibuffer-prompt-properties
      '(read-only t
        intangible t
        cursor-intangible t
        face
        minibuffer-prompt))

(add-hook 'minibuffer-setup-hook #'pkg-minibuffer/change-cursor-type)
(add-hook 'minibuffer-setup-hook #'pkg-minibuffer/truncate-lines)
(add-hook 'minibuffer-setup-hook #'cursor-intangible-mode)
(add-hook 'after-init-hook #'file-name-shadow-mode)
(add-hook 'after-init-hook #'minibuffer-electric-default-mode)

;;; Misc

(defun my/escape ()
  "Run `my/escape-hook'."
  (interactive)
  (cond (;; Quit the minibuffer if open.
         (minibuffer-window-active-p (minibuffer-window))
         (abort-recursive-edit))

        ;; Run all escape hooks. If any returns non-nil, then stop there.
        ((run-hook-with-args-until-success 'my/escape-hook))

        ;; Don't abort macros.
        ((or defining-kbd-macro executing-kbd-macro) nil)

        ;; Back to the default.
        ((keyboard-quit))))

(defun my/protect-buffer ()
  "Don't kill certain buffers. Meant for `kill-buffer-query-functions'.
If this function returns true then the buffer can be killed."
  (let ((buffer-name (buffer-name)))
    (if (equal buffer-name "*Messages*")
        (progn (bury-buffer) nil)
      t)))

(defun my/remove-horizontal-scroll-margin-in-shells ()
  (setq-local hscroll-margin 0))

;; Make sure the cache directory exists.
(when (not (file-directory-p (concat my/cache-dir)))
  (make-directory my/cache-dir t))

;; Increase the maximum number of lines to keep in the message log buffer.
(setq message-log-max 8192)

;; UTF-8 as the default coding system.
(set-charset-priority 'unicode)
(prefer-coding-system 'utf-8)
(setq locale-coding-system 'utf-8
      selection-coding-system 'utf-8
      default-process-coding-system '(utf-8 . utf-8))

;; Make apropos omnipotent. It's more useful this way.
(setq apropos-do-all t)

;; Indentation.
(setq-default tab-width 2
              indent-tabs-mode nil
              fill-column 80)

;; Use TAB to trigger completion.
(setq-default tab-first-completion nil ; complete
              tab-always-indent 'complete)

;; Enable new Emacs 27 native fill column mode, but I currently have it
;; disabled.
;; (global-display-fill-column-indicator-mode +1)

;; Always request confirmation before switching to a nonexistent buffer. I
;; usually prefer to use the *untitled* buffer instead.
(setq confirm-nonexistent-file-or-buffer t)

;; Do not automatically save changes.
(setq auto-save-default nil
      auto-save-list-file-name (concat my/cache-dir "autosave"))

(setq make-backup-files t)
(setq vc-make-backup-files t) ; Backup version controlled files
(setq backup-by-copying t)    ; Don't clobber symlinks
(setq delete-old-versions t)  ; Don't ask about deleting old versions
(setq kept-new-versions 10)   ; Keep 10 latest versions
(setq kept-old-versions 2)    ; Keep 2 oldest versions
(setq version-control t)      ; Number backups

;; Avoid littering the user's filesystem with backups.
(setq backup-directory-alist
      `((".*" . ,temporary-file-directory)))

;; Do not use lockfiles to avoid editing collisions.
(setq create-lockfiles nil)

;; Enable folding by indentation, just like Vim when using Evil.
(add-hook 'prog-mode-hook #'hs-minor-mode)

;; Change all cursor movement/edit commands to stop in-between the "camelCase"
;; words.
(add-hook 'prog-mode-hook #'subword-mode)

;; Don't allow certain buffers to be killed.
(add-hook 'kill-buffer-query-functions #'my/protect-buffer)

;; Avoid problems with crontabs, diffs, etc. A value of t means only add a
;; newline when the file is about to be saved.
(setq mode-require-final-newline t)

;; Changes all yes/no questions to y/n type.
(setq use-short-answers t)

;; I don't close Emacs very often, but when I do it's a mistake ;)
(setq confirm-kill-emacs #'y-or-n-p)

;; Emacs stores authinfo in HOME and in plaintext, so let's use an encrypted
;; file instead.
(setq auth-sources (list (expand-file-name "~/.authinfo.gpg")))

;; Don't compact font caches during GC. Disabling compaction of font caches
;; might enlarge the Emacs memory footprint in sessions that use lots of
;; different fonts.
(setq inhibit-compacting-font-caches t)

;; Introduced in Emacs HEAD (b2f8c9f), this inhibits fontification while
;; receiving input, which should help a little with scrolling performance.
(setq redisplay-skip-fontification-on-input t)

;; Prevent the cursor from blinking. Do it two ways: using the minor
;; mode only works during regular init, while using the variable
;; only works during early init.
(blink-cursor-mode -1)
(setq no-blinking-cursor nil)
(setq blink-cursor-blinks 8)
(setq blink-cursor-interval 0.6)
(setq-default cursor-type 'bar)

;; Don't stretch the cursor to fit wide characters, it is disorienting,
;; especially for tabs.
(setq x-stretch-cursor nil)

;; Do not make the cursor "very visible". This only has an effect when running
;; in a text terminal. What means "very visible" is up to your terminal. It
;; may make the cursor bigger, or it may make it blink, or it may do nothing
;; at all.
(setq visible-cursor nil)

;; No frame title.
(setq-default frame-title-format nil)

;; No file fialog.
(setq use-file-dialog nil)

;; No bell.
(setq ring-bell-function 'ignore)

;; Configure vertical space between lines.
(setq-default line-spacing 0)

;; Reduce clutter in the fringes.
(setq indicate-buffer-boundaries nil
      indicate-empty-lines nil)

;; Set default without continuation and truncation indicators.
(setq-default fringe-indicator-alist
              '((overlay-arrow . right-triangle)
                (up . up-arrow)
                (down . down-arrow)
                (top top-left-angle top-right-angle)
                (bottom bottom-left-angle bottom-right-angle top-right-angle top-left-angle)
                (top-bottom left-bracket right-bracket top-right-angle top-left-angle)
                (unknown . question-mark)))

(setq-default show-trailing-whitespace nil)

;; The native border consumes a pixel of the fringe on righter-most splits,
;; `window-divider' does not.
(setq window-divider-default-places 'right-only
      window-divider-default-bottom-width 1
      window-divider-default-right-width 1)

;; Enable mouse in terminal Emacs.
(add-hook 'tty-setup-hook #'xterm-mouse-mode)

(when my/mac?
  ;; Sane trackpad/mouse scroll settings.
  (setq mac-redisplay-dont-reset-vscroll t
        mac-mouse-wheel-smooth-scroll nil))

;; Remove hscroll-margin in shells, otherwise it causes jumpiness.
(dolist (mode '(eshell-mode-hook term-mode-hook vterm-mode-hook))
  (add-hook mode #'my/remove-horizontal-scroll-margin-in-shells))

;; Always avoid GUI.
(setq use-dialog-box nil)

;; Don't display floating tooltips; display their contents in the echo-area,
;; because native tooltips are ugly.
(when (bound-and-true-p tooltip-mode)
  (tooltip-mode -1))
(when my/linux?
  (setq x-gtk-use-system-tooltips nil))

;; Favor vertical splits over horizontal ones. Screens are usually wide.
(setq split-width-threshold 120
      split-height-threshold nil)

;; Show current key-sequence in minibuffer, like vim does. Reduce the default
;; time of 1s.
(setq echo-keystrokes 1e-6)

;; No word-wrap
(setq-default truncate-lines t
              truncate-partial-width-windows nil)

;; Only print error-related messages.
(setq save-silently t)

;; Remove trailing whitespace before saving.
(add-hook 'before-save-hook #'whitespace-cleanup)

;; Follow symlinks without asking. The default configuration is safer, but I use
;; symlinks extensively.
(setq vc-follow-symlinks t
      find-file-visit-truename t)

;; The `project' library requires the git backend.
(setq vc-handled-backends '(Git))

;; Disable the warning "X and Y are the same file". It's fine to ignore this
;; warning as it will redirect you to the existing buffer anyway.
(setq find-file-suppress-same-file-warnings t)

;; Always select the help window, because usually I just want to take a quick
;; look and close it with q.
(setq help-window-select t)

(defun my/quit-window (_original-fn &optional bury window)
  "Quit WINDOW and kill its buffer.
It has the same implementation as the original `quit-window', but
with 'kill' as the default action instead of 'bury'."
  (interactive "P")
  (with-current-buffer (window-buffer (window-normalize-window window))
    (run-hooks 'quit-window-hook))
  (quit-restore-window window (if bury 'bury 'kill)))

;; I prefer to always kill the window's buffer.
(advice-add 'quit-window :around #'my/quit-window)

;; Enable pop-up windows when `display-buffer' is called.
(setq pop-up-windows t)

;; Starting on Emacs 27+ theme changes don't take effect immediately.
(setq custom--inhibit-theme-enable nil)

;; How to construct unique buffer names for files with the same base name.
(setq uniquify-buffer-name-style 'forward)

;; Middle-click paste at point, not at click.
(setq mouse-yank-at-point t)

;; Underline looks a bit better when drawn lower
(setq x-underline-at-descent-line t)

;; Use one space between sentences.
(setq sentence-end-double-space nil)

;; Deleting files go to OS's trash folder.
(setq delete-by-moving-to-trash t)

;; Use the maximum decoration available.
(setq font-lock-maximum-decoration t)

;; Increase the amount of data which Emacs reads from a process. The default of
;; 4KB is too low considering that some of the LSP responses are in the 800KB -
;; 3MB range.
(setq read-process-output-max (* 1024 1024))

;; Enable improved escape.
(global-set-key [remap keyboard-quit] #'my/escape)

;; Variable that are considered safe. `safe-local-variable-values' is explicitly
;; set for future reference.
(setq safe-local-variable-values nil)
(put 'line-spacing 'safe-local-variable #'integerp)
(put 'visual-fill-column-center-text 'safe-local-variable #'booleanp)

;; https://www.gnu.org/software/emacs/manual/html_node/emacs/Completion-Styles.html
;; Original value: '(basic partial-completion emacs22)
;; Option 'basic is the fastest and 'substring can be somewhat slow.
(setq completion-styles '(basic partial-completion))

;; Expressions that are considered safe in an "eval:" local variable, i.e.
;; Emacs won't ask you for confirmation. Commented out for future reference.
;; (add-to-list 'safe-local-eval-forms '(my/safe-function arg1 arg2))

;; For unknown reasons, Emacs don't understand dead keys and "cedilha" (ç, used
;; by brazilians) after upgrading to version 27.1 (using the GNU Guix package).
;; Requiring `iso-transl' is enough to make most dead keys work, but I still
;; need this keybinding hardcoded. Other solutions are more invasive, like
;; changing the XMODIFIERS environment variable.
(require 'iso-transl)
(global-set-key (kbd "<dead-acute> c") (lambda () (interactive) (insert "ç")))

;; Disable warning threshold. Usually bigger files are opened with an external
;; application configured with the `openwith' package.
(setq large-file-warning-threshold nil)

;; Enable all disabled commands.
;; https://www.emacswiki.org/emacs/DisabledCommands
(setq disabled-command-function nil)

;; Do not try to detect JSX syntax because we'll always use file extensions.
(setq js-jsx-detect-syntax nil)

;; Disable warnings from legacy advice system. They aren't useful, and what can
;; we do about them, besides changing packages upstream?
(setq ad-redefinition-action 'accept)

;; Disable output messages whenever compressing or uncompressing files.
(setq jka-compr-verbose nil)

;; Inhibit loading the default library.
(setq inhibit-default-init t)

(setq gamegrid-user-score-file-directory (concat my/cache-dir "games/"))

;; Show outlines in the describe bindings buffer.
(setq describe-bindings-outline t)

;;; Modeline

;; Prevent flash of unstyled modeline at startup. The styled modeline will be
;; loaded much later.
(defvar my/original-mode-line-format nil)
(unless after-init-time
  (setq my/original-mode-line-format mode-line-format)
  (setq-default mode-line-format nil))

;;; Native compilation

;; Non-nil to prevent native-compiling of Emacs Lisp code.
(setq no-native-compile nil)

;; This is the recommended value unless you are debugging the compiler itself.
(setq native-comp-debug 0)

;; Choose to prevent unwanted runtime compilation for gccemacs (native-comp)
;; users; packages are compiled ahead-of-time when they are installed and site
;; files are compiled when gccemacs is installed.
;;
;; Compile AOT with:
;;  (native-compile-async "~/.emacs.d/.local/straight/build" 'recursively)
(setq native-comp-deferred-compilation nil)

;; This intended for debugging the compiler itself.
;;   - 0 no logging.
;;   - 1 final limple is logged.
;;   - 2 LAP and final limple and some pass info are logged.
;;   - 3 max verbosity.
(setq native-comp-verbose 0)

(setq native-comp-async-report-warnings-errors 'silent)

(setq native-compile-target-directory (concat my/cache-dir "eln/"))

;;; Python

;; In Python it's very unsafe to assume the indentation offset is 2/4/etc.
;; Instead, let Emacs guess the offset, but don't print any message.
(setq python-indent-guess-indent-offset t
      python-indent-guess-indent-offset-verbose nil)

;;; Shell

;; Solves issues with commands such as "git log" and the default "less".
(setenv "PAGER" "cat")

(defun my/exec-path-prepend (path)
  "Prepend PATH to `exec-path' if not present already."
  (let ((path (expand-file-name path)))
    (unless (member path exec-path)
      (setq exec-path (cons path exec-path)))))

;; Tell Emacs where global NPM packages are installed. For a more general
;; approach, get the prefix path using "npm config get prefix", but do it
;; asynchronously, as this command is rather slow.
;;
;; This workaround wouldn't be necessary if Emacs GUI could start with the
;; correct $PATH. I prefer to avoid duplicating what's sourced from
;; shell/variables to ~/.profile.
(my/exec-path-prepend "~/.local/opt/npm-packages/bin")

(defun pkg-shell-mode/use-read-only-mode-in-shell-command-buffers ()
  (when-let ((buffer (get-buffer "*Async Shell Command*")))
    (with-current-buffer buffer
      (read-only-mode +1))))

;; I usually use shell command buffers to read text, but not change it.
(add-hook 'shell-mode-hook #'pkg-shell-mode/use-read-only-mode-in-shell-command-buffers)

;;; Simple

;; Eliminate duplicates in the kill ring. That is, if you kill the same thing
;; twice, you won't have to use M-y twice to get past it to older entries in the
;; kill ring.
(setq kill-do-not-save-duplicates t)

;;; Uniquify buffer names

(setq uniquify-buffer-name-style 'reverse)
(setq uniquify-separator " • ")
(setq uniquify-after-kill-buffer-p t)
(setq uniquify-ignore-buffers-re "^\\*")

;;; X11

;; Ignore X resources; its settings would be redundant with the other settings
;; in this file and can conflict with later config.
(advice-add #'x-apply-session-resources :override #'ignore)

;;; Window

(defun pkg-window/display-buffer-reuse-window (original-fn &rest args)
  "Force `switch-to-buffer' to respect display buffer actions and
reuse the same window whenever possible."
  (let ((display-buffer-overriding-action (when my/reuse-buffer-window-p
                                            '((display-buffer-reuse-window)))))
    (apply original-fn args)))

(defun pkg-window/resize-left (arg)
  (interactive "p")
  (if (let ((windmove-wrap-around))
        (windmove-find-other-window 'right))
      (shrink-window-horizontally arg)
    (enlarge-window-horizontally arg)))

(defun pkg-window/resize-down (arg)
  (interactive "p")
  (if (let ((windmove-wrap-around))
        (windmove-find-other-window 'up))
      (shrink-window arg)
    (enlarge-window arg)))

(defun pkg-window/resize-up (arg)
  (interactive "p")
  (if (let ((windmove-wrap-around))
        (windmove-find-other-window 'up))
      (enlarge-window arg)
    (shrink-window arg)))

(defun pkg-window/resize-right (arg)
  (interactive "p")
  (if (let ((windmove-wrap-around nil))
        (windmove-find-other-window 'right))
      (enlarge-window-horizontally arg)
    (shrink-window-horizontally arg)))

(transient-define-prefix pkg-window/transient-resize ()
  "Transients to resize windows."
  ["Resize window"
   [("h" "←" pkg-window/resize-left :transient t)]
   [("j" "↓" pkg-window/resize-down :transient t)]
   [("k" "↑" pkg-window/resize-up :transient t)]
   [("l" "→" pkg-window/resize-right :transient t)]])

;;;; Autoloads

;;;###autoload
(defun pkg-window/split-window-right (&optional size)
  "Split window right and select the newly split-off window."
  (interactive "P")
  (let ((new-window (split-window-horizontally size)))
    (select-window new-window)
    (set-window-start new-window (window-start (previous-window)))))

;;;###autoload
(defun pkg-window/split-window-below (&optional size)
  "Split window below and select the newly split-off window."
  (interactive "P")
  (let ((new-window (split-window-vertically size)))
    (select-window new-window)
    (set-window-start new-window (window-start (previous-window)))))

;;;; Keybindings

(defvar pkg-emacs/window-command-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "+")   #'maximize-window)
    (define-key map (kbd "0")   #'pkg-window/transient-resize)
    (define-key map (kbd "=")   #'balance-windows)
    (define-key map (kbd "C-r") #'winner-redo)
    (define-key map (kbd "C-w") #'evil-window-next)
    (define-key map (kbd "h")   #'evil-window-left)
    (define-key map (kbd "j")   #'evil-window-down)
    (define-key map (kbd "k")   #'evil-window-up)
    (define-key map (kbd "l")   #'evil-window-right)
    (define-key map (kbd "o")   #'delete-other-windows)
    (define-key map (kbd "q")   #'evil-quit)
    (define-key map (kbd "u")   #'winner-undo)
    (define-key map (kbd "s")   #'pkg-window/split-window-below)
    (define-key map (kbd "v")   #'pkg-window/split-window-right)
    (define-key map (kbd "|")   #'evil-window-set-width)
    (define-key map (kbd "_")   #'evil-window-set-height)
    map))
(fset 'pkg-emacs/window-command-map pkg-emacs/window-command-map)

(define-key my/keys-mode-map (kbd (concat my/windmove-modifier "-h")) #'windmove-left)
(define-key my/keys-mode-map (kbd (concat my/windmove-modifier "-j")) #'windmove-down)
(define-key my/keys-mode-map (kbd (concat my/windmove-modifier "-k")) #'windmove-up)
(define-key my/keys-mode-map (kbd (concat my/windmove-modifier "-l")) #'windmove-right)

(global-set-key [remap split-window-right] #'pkg-window/split-window-right)
(global-set-key [remap split-window-below] #'pkg-window/split-window-below)

;;;; Init

;; Leave the window configuration alone, i.e. don't try to even window sizes.
(setq even-window-sizes nil)

(setq window-combination-resize t)
(setq even-window-sizes 'height-only)
(setq switch-to-buffer-in-dedicated-window 'pop)
(setq switch-to-buffer-obey-display-actions t)

;; When nil, top and bottom side windows occupy full frame width.
(setq window-sides-vertical nil)

;; This is the default action used by display-buffer if no other actions are
;; specified or all fail, before falling back on
;; display-buffer-fallback-action.
;;
;; Perspective.el is one such package that recommends users to stop the whole
;; window madness, where carefully created window configurations are destroyed
;; by calling commands like `project-compile'. To bring a bit more sanity to
;; window management, I prefer to reuse existing windows whenever possible, and
;; otherwise use the current window to display the buffer in question.
;;
;; It also means I don't need to carefully craft `display-buffer-alist'.
(setq display-buffer-base-action
      '((display-buffer-reuse-window
         display-buffer-same-window)

        ;; Seach all frames for a reusable window.
        (reusable-frames . t)))

;; Left here as a reference for future needs.
;;
;; (setq display-buffer-alist
;;       `((,(rx "*Proced*")
;;          (display-buffer-reuse-window
;;           display-buffer-same-window))
;;         (,(rx "*Process List*")
;;          (display-buffer-reuse-window
;;           display-buffer-same-window))
;;         (,(rx "*Help*")
;;          (display-buffer-reuse-window
;;           display-buffer-same-window))
;;         (,(rx "*helpful" (one-or-more not-newline))
;;          (display-buffer-reuse-window
;;           display-buffer-same-window))
;;         (,(rx line-start "magit: ")
;;          (display-buffer-reuse-window
;;           display-buffer-same-window))
;;         ;; (,(rx "*"
;;         ;;    (zero-or-more not-newline)
;;         ;;    (or "eshell" "shell" "term" "vterm")
;;         ;;    (zero-or-more not-newline))
;;         ;;  (display-buffer-reuse-mode-window
;;         ;;   display-buffer-below-selected))
;;         (,(rx "*Embark Actions*")
;;          (display-buffer-reuse-mode-window
;;           display-buffer-at-bottom)
;;          (window-height . fit-window-to-buffer)
;;          (window-parameters . ((no-other-window . t)
;;                                (mode-line-format . none))))))

;;;; Config

;; Note: the same technique can't be applied to `next-buffer' or `previous-buffer'.
(advice-add #'switch-to-buffer :around #'pkg-window/display-buffer-reuse-window)
