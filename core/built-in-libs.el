;;; -*- lexical-binding: t; -*-

(defgroup built-in-libs nil
  "Configuration for built-in packages."
  :group 'my
  :prefix "built-in-libs/")

;;; Auto-revert

;; Only prompts for confirmation when buffer is unsaved.
(setq revert-without-query '("."))

(setq auto-revert-verbose t
      auto-revert-stop-on-user-input nil
      auto-revert-use-notify nil)

;;; Battery

(setq battery-mode-line-limit 99
      battery-update-interval 60
      battery-load-low 20
      battery-load-critical 10)

;;; Bookmark

;; File in which to save bookmarks by default.
(setq bookmark-default-file (concat my/cache-dir "bookmarks"))

(setq bookmark-set-fringe-mark t)

;;; Compilation

;; Don't ask for the compilation command in the minibuffer. I often prefer to
;; set this variable in a directory locals file.
(setq compilation-read-command nil)

;;; Diff

(setq diff-default-read-only t)

;;; Dired
;;;; Variables

(defvar-local my/dired-hidden-show-p nil)

;; Enable `dired-hide-details-mode' by default.
(defvar pkg-dired/global-hide-details-mode t)

(defun pkg-dired/setup-mode-h ()
  "Toggle `dired-hide-details-mode' based on global var."
  (if pkg-dired/global-hide-details-mode
      (dired-hide-details-mode +1)
    (dired-hide-details-mode -1)))

;;;; Autoloads

;;;###autoload
(defun pkg-dired/hide-details-mode ()
  "Toggle `dired-hide-details-mode' globally."
  (interactive)
  (if (bound-and-true-p dired-hide-details-mode)
      (progn
        (setq pkg-dired/global-hide-details-mode nil)
        (dired-hide-details-mode -1))
    (dired-hide-details-mode +1)
    (setq pkg-dired/global-hide-details-mode t)))

;;;###autoload
(defun pkg-dired/toggle-hidden ()
  "Show/hide dot-files."
  (interactive)
  (when (equal major-mode 'dired-mode)
    (if my/dired-hidden-show-p
        ;; If currently showing.
        (progn
          (dired-mark-files-regexp "^\\\.")
          (dired-do-kill-lines))
      ;; Otherwise just revert to re-show.
      (progn (revert-buffer)
             (setq-local my/dired-hidden-show-p t)))))

;;;; Init

(add-hook 'dired-mode-hook #'pkg-dired/setup-mode-h)

(transient-define-prefix pkg-dired/transient ()
  :transient-non-suffix #'transient--do-stay
  [[:description "UI"
    ("(" "Toggle details" dired-hide-details-mode :transient t)
    ("h" "Toggle hidden files" pkg-dired/toggle-hidden :transient t)
    ("U" "Remove all marks" dired-unmark-all-marks :transient t)
    ("m" "Mark file" dired-mark :transient t)
    ("u" "Unmark file" dired-unmark :transient t)]
   [:description "Permissions"
    ("G" "Change group" dired-do-chgrp)
    ("M" "Change mode" dired-do-chmod)
    ("O" "Change owner" dired-do-chown)]
   [:description "File system"
    ("+" "Create directory" dired-create-directory)
    ("C" "Copy" dired-do-copy)
    ("D" "Delete" dired-do-delete)
    ("R" "Rename" dired-do-rename)]]
  [:description "View"
   :pad-keys t
   ("v"     "Examine file in view mode" dired-view-file)
   ("M-RET" "Display file or directory in another window" dired-display-file)
   ("!"     "Run shell command on marked files" dired-do-shell-command)
   ("Z"     "Compress or uncompress marked files" dired-do-compress)
   ("; e"   "Encrypt marked files" epa-dired-do-encrypt)
   ("; d"   "Decrypt marked files" epa-dired-do-decrypt)])

;; Don't pass "--dired" flag to "ls".
(setq dired-use-ls-dired nil)

;; Always copy/delete recursively.
(setq dired-recursive-copies 'always
      dired-recursive-deletes 'top)

(setq dired-compress-directory-default-suffix ".tar.gz"
      dired-compress-file-default-suffix ".gz")

(setq dired-auto-revert-buffer t
      dired-do-revert-buffer t
      dired-dwim-target t  ; Suggest a target for moving/copying intelligently
      dired-hide-details-hide-symlink-targets nil
      dired-listing-switches "-lAFh")

;; If non-nil, kill the current buffer when selecting a new directory.
(setq dired-kill-when-opening-new-dired-buffer t)

;; Disable the prompt about whether I want to kill the Dired buffer for a
;; deleted directory.
(setq dired-clean-confirm-killing-deleted-buffers nil)

(when (version= "29.0.50" emacs-version)
  (setq dired-free-space nil))

(setq
 ;; Emulate the GNU platform.
 ls-lisp-emulation nil
 ls-lisp-dirs-first t

 ;; Do not use external ls program.
 ls-lisp-use-insert-directory-program nil)

;;; Emacs Lisp Mode
;;;; Private
(defun pkg-elisp-mode/list-at-point (&optional bounds)
  "Return the list (compound form) at point as a string, otherwise nil.
If BOUNDS is non-nil, return a list of its starting and ending position
instead. Copied from `cider-list-at-point'."
  (when-let* ((b (or (and (equal (char-after) ?\()
                          (member (char-before) '(?\' ?\, ?\@))
                          ;; hide stuff before ( to avoid quirks with '( etc.
                          (save-restriction
                            (narrow-to-region (point) (point-max))
                            (bounds-of-thing-at-point 'list)))
                     (bounds-of-thing-at-point 'list))))
    (funcall (if bounds #'list #'buffer-substring-no-properties)
             (car b) (cdr b))))

(defun pkg-elisp-mode/calculate-lisp-indent (&optional parse-start)
  "Add better indentation for quoted and backquoted lists.
Taken from https://emacs.stackexchange.com/questions/10230/how-to-indent-keywords-aligned"
  ;; This line because `calculate-lisp-indent-last-sexp` was defined with `defvar`
  ;; with it's value ommited, marking it special and only defining it locally. So
  ;; if you don't have this, you'll get a void variable error.
  (defvar calculate-lisp-indent-last-sexp)
  (save-excursion
    (beginning-of-line)
    (let ((indent-point (point))
          state
          ;; setting this to a number inhibits calling hook
          (desired-indent nil)
          (retry t)
          calculate-lisp-indent-last-sexp containing-sexp)
      (cond ((or (markerp parse-start) (integerp parse-start))
             (goto-char parse-start))
            ((null parse-start) (beginning-of-defun))
            (t (setq state parse-start)))
      (unless state
        ;; Find outermost containing sexp
        (while (< (point) indent-point)
          (setq state (parse-partial-sexp (point) indent-point 0))))
      ;; Find innermost containing sexp
      (while (and retry
                  state
                  (> (elt state 0) 0))
        (setq retry nil)
        (setq calculate-lisp-indent-last-sexp (elt state 2))
        (setq containing-sexp (elt state 1))
        ;; Position following last unclosed open.
        (goto-char (1+ containing-sexp))
        ;; Is there a complete sexp since then?
        (if (and calculate-lisp-indent-last-sexp
                 (> calculate-lisp-indent-last-sexp (point)))
            ;; Yes, but is there a containing sexp after that?
            (let ((peek (parse-partial-sexp calculate-lisp-indent-last-sexp
                                            indent-point 0)))
              (if (setq retry (car (cdr peek))) (setq state peek)))))
      (if retry
          nil
        ;; Innermost containing sexp found
        (goto-char (1+ containing-sexp))
        (if (not calculate-lisp-indent-last-sexp)
            ;; indent-point immediately follows open paren.
            ;; Don't call hook.
            (setq desired-indent (current-column))
          ;; Find the start of first element of containing sexp.
          (parse-partial-sexp (point) calculate-lisp-indent-last-sexp 0 t)
          (cond ((looking-at "\\s(")
                 ;; First element of containing sexp is a list.
                 ;; Indent under that list.
                 )
                ((> (save-excursion (forward-line 1) (point))
                    calculate-lisp-indent-last-sexp)
                 ;; This is the first line to start within the containing sexp.
                 ;; It's almost certainly a function call.
                 (if (or
                      ;; Containing sexp has nothing before this line
                      ;; except the first element. Indent under that element.
                      (= (point) calculate-lisp-indent-last-sexp)

                      ;; First sexp after `containing-sexp' is a keyword. This
                      ;; condition is more debatable. It's so that I can have
                      ;; unquoted plists in macros. It assumes that you won't
                      ;; make a function whose name is a keyword.
                      (when-let (char-after (char-after (1+ containing-sexp)))
                        (char-equal char-after ?:))

                      ;; Check for quotes or backquotes around.
                      (let* ((positions (elt state 9))
                             (last (car (last positions)))
                             (rest (reverse (butlast positions)))
                             (any-quoted-p nil)
                             (point nil))
                        (or
                         (when-let (char (char-before last))
                           (or (char-equal char ?')
                               (char-equal char ?`)))
                         (progn
                           (while (and rest (not any-quoted-p))
                             (setq point (pop rest))
                             (setq any-quoted-p
                                   (or
                                    (when-let (char (char-before point))
                                      (or (char-equal char ?')
                                          (char-equal char ?`)))
                                    (save-excursion
                                      (goto-char (1+ point))
                                      (looking-at-p
                                       "\\(?:back\\)?quote[\t\n\f\s]+(")))))
                           any-quoted-p))))
                     ;; Containing sexp has nothing before this line
                     ;; except the first element.  Indent under that element.
                     nil
                   ;; Skip the first element, find start of second (the first
                   ;; argument of the function call) and indent under.
                   (progn (forward-sexp 1)
                          (parse-partial-sexp (point)
                                              calculate-lisp-indent-last-sexp
                                              0 t)))
                 (backward-prefix-chars))
                (t
                 ;; Indent beneath first sexp on same line as
                 ;; `calculate-lisp-indent-last-sexp'.  Again, it's
                 ;; almost certainly a function call.
                 (goto-char calculate-lisp-indent-last-sexp)
                 (beginning-of-line)
                 (parse-partial-sexp (point) calculate-lisp-indent-last-sexp
                                     0 t)
                 (backward-prefix-chars)))))
      ;; Point is at the point to indent under unless we are inside a string.
      ;; Call indentation hook except when overridden by lisp-indent-offset
      ;; or if the desired indentation has already been computed.
      (let ((normal-indent (current-column)))
        (cond ((elt state 3)
               ;; Inside a string, don't change indentation.
               nil)
              ((and (integerp lisp-indent-offset) containing-sexp)
               ;; Indent by constant offset
               (goto-char containing-sexp)
               (+ (current-column) lisp-indent-offset))
              ;; in this case calculate-lisp-indent-last-sexp is not nil
              (calculate-lisp-indent-last-sexp
               (or
                ;; try to align the parameters of a known function
                (and lisp-indent-function
                     (not retry)
                     (funcall lisp-indent-function indent-point state))
                ;; If the function has no special alignment
                ;; or it does not apply to this argument,
                ;; try to align a constant-symbol under the last
                ;; preceding constant symbol, if there is such one of
                ;; the last 2 preceding symbols, in the previous
                ;; uncommented line.
                (and (save-excursion
                       (goto-char indent-point)
                       (skip-chars-forward " \t")
                       (looking-at ":"))
                     ;; The last sexp may not be at the indentation
                     ;; where it begins, so find that one, instead.
                     (save-excursion
                       (goto-char calculate-lisp-indent-last-sexp)
                       ;; Handle prefix characters and whitespace
                       ;; following an open paren.  (Bug#1012)
                       (backward-prefix-chars)
                       (while (not (or (looking-back "^[ \t]*\\|([ \t]+"
                                                     (line-beginning-position))
                                       (and containing-sexp
                                            (>= (1+ containing-sexp) (point)))))
                         (forward-sexp -1)
                         (backward-prefix-chars))
                       (setq calculate-lisp-indent-last-sexp (point)))
                     (> calculate-lisp-indent-last-sexp
                        (save-excursion
                          (goto-char (1+ containing-sexp))
                          (parse-partial-sexp (point) calculate-lisp-indent-last-sexp 0 t)
                          (point)))
                     (let ((parse-sexp-ignore-comments t)
                           indent)
                       (goto-char calculate-lisp-indent-last-sexp)
                       (or (and (looking-at ":")
                                (setq indent (current-column)))
                           (and (< (line-beginning-position)
                                   (prog2 (backward-sexp) (point)))
                                (looking-at ":")
                                (setq indent (current-column))))
                       indent))
                ;; another symbols or constants not preceded by a constant
                ;; as defined above.
                normal-indent))
              ;; in this case calculate-lisp-indent-last-sexp is nil
              (desired-indent)
              (t
               normal-indent))))))

(defun pkg-elisp-mode/outline-minor-mode-h ()
  (when (derived-mode-p 'emacs-lisp-mode)
    (setq-local outline-regexp my/outline-regex-lisp)))

;;;; Autoloads

;;;###autoload
(defun pkg-elisp-mode/eval-list-at-point ()
  "Evaluate the list (eg. a function call, surrounded by parens)
around point."
  (interactive)
  (save-excursion
    (goto-char (cadr (pkg-elisp-mode/list-at-point 'bounds)))
    (if (fboundp 'eros-eval-last-sexp)
        (call-interactively #'eros-eval-last-sexp)
      (call-interactively #'eval-last-sexp))))


;;;###autoload
(defun pkg-elisp-mode/run-file-tests ()
  "Run all tests in the current buffer."
  (interactive)
  (let* ((base-name (file-name-base buffer-file-name))
         (prefix (progn
                   (string-match "^\\(.+-\\)test$" base-name)
                   (match-string 1 base-name))))
    (ert (concat "^" prefix))))

;;;###autoload
(defun pkg-elisp-mode/run-project-tests ()
  "Run all tests prefixed with the current project name."
  (interactive)
  (let* ((root-path (project-root (project-current)))
         (dir-name (file-name-nondirectory (directory-file-name root-path))))
    (ert (format "^%s-" dir-name))))

;;;###autoload
(defun pkg-elisp-mode/pp-eval-defun-as-json-other-window ()
  "Pretty-print eval'ed JSON string in another buffer."
  (interactive)
  (let ((result (let ((inhibit-message t))
                  (elisp--eval-defun))))
    (with-current-buffer
        (switch-to-buffer-other-window "*Pretty-print JSON*")
      (read-only-mode -1)
      (erase-buffer)
      (insert result)
      (json-mode)
      (call-interactively #'json-pretty-print-buffer)
      (read-only-mode +1))))

;;;###autoload
(defun pkg-elisp-mode/eval-buffer ()
  (interactive)
  (call-interactively #'eval-buffer)
  (message "Evaluated '%s'." (buffer-name)))

;;;###autoload
(defun pkg-elisp-mode/eval-defun-2nd-symbol ()
  "Eval 2nd symbol starting from the beginning of defun.

This is particularly useful to evaluate the value of a var."
  (interactive)
  (save-excursion
    (beginning-of-defun)
    (forward-symbol 2)
    (call-interactively #'eval-last-sexp)))

;;;; Init

;; Although this checker is very useful for certain kinds of projects
;; (e.g. package maintainers), I prefer to disable the checkdoc
;; because it's extremely annoying most of the time. I don't really
;; care about adding a 'Commentary' section to every elisp file, for
;; example.
(add-hook 'emacs-lisp-mode-hook #'outline-minor-mode)
(add-hook 'outline-minor-mode-hook #'pkg-elisp-mode/outline-minor-mode-h)

(add-hook 'emacs-lisp-mode-hook #'cl-font-lock-built-in-mode)

;;;; Config

(advice-add #'calculate-lisp-indent :override #'pkg-elisp-mode/calculate-lisp-indent)

;;; Eldoc

(setq eldoc-idle-delay 0.25
      eldoc-minor-mode-string nil
      eldoc-echo-area-use-multiline-p nil)

;;; Eshell
;;;; Faces

(defface pkg-eshell/prompt-pwd '((t (:inherit font-lock-constant-face)))
  "Face for current working directory."
  :group 'eshell)

(defface pkg-eshell/prompt-git-branch '((t (:inherit font-lock-builtin-face)))
  "Face for git branch."
  :group 'eshell)

;;;; Custom

(defcustom pkg-eshell/buffer-name "*eshell toggle*"
  "Buffer name to pop eshell."
  :type 'string)

(defcustom pkg-eshell/kill-window-on-exit nil
  "If non-nil, eshell will close windows along with its eshell buffers."
  :type 'string)

(defcustom pkg-eshell/enable-new-shell-on-split t
  "If non-nil, spawn a new eshell session after splitting from an
eshell buffer."
  :type 'boolean)

(defcustom pkg-eshell/buffer-name-separator " • "
  "String used to separate `eshell-buffer-name' and the current
working directory (`default-directory')."
  :type 'string
  :group 'eshell)

;;;; Private

(defun pkg-eshell/-last-input ()
  (when (and eshell-last-input-start eshell-last-input-end)
    (let ((input (string-trim-right (buffer-substring-no-properties eshell-last-input-start eshell-last-input-end))))
      (if (string-blank-p input)
          nil
        input))))

(defun pkg-eshell/-current-git-branch ()
  (cl-destructuring-bind (status . output)
      (lib-util/call-process "git symbolic-ref -q --short HEAD")
    (if (equal status 0)
        (format " [%s]" output)
      (cl-destructuring-bind (status . output)
          (lib-util/call-process "git describe --all --always HEAD")
        (if (equal status 0)
            (format " [%s]" output)
          "")))))

(defun pkg-eshell/-prompt-p ()
  (let ((line (buffer-substring-no-properties (line-beginning-position) (line-end-position))))
    (string-match-p eshell-prompt-regexp line)))

;;;; Public
;;;;; Functions

(defun pkg-eshell/rename-buffer-with-last-input ()
  "Rename current eshell buffer with last input."
  (when-let ((last-input (pkg-eshell/-last-input)))
    (let* ((separator pkg-eshell/buffer-name-separator)
           (separator-regexp (rx (literal separator)
                                 (group (one-or-more not-newline))
                                 line-end))
           (current-name (buffer-name))
           (new-name (generate-new-buffer-name (if (string-match-p separator-regexp current-name)
                                                   (replace-regexp-in-string separator-regexp
                                                                             (concat separator last-input)
                                                                             current-name)
                                                 (concat current-name separator last-input)))))
      (rename-buffer new-name))))

(defun pkg-eshell/default-prompt ()
  "Generate the prompt string for eshell. Use for `eshell-prompt-function'."
  (concat (if (bobp) "" "\n")
          (let ((pwd (eshell/pwd)))
            (propertize (if (equal pwd "~")
                            pwd
                          (abbreviate-file-name (shrink-path-file pwd)))
                        'face 'pkg-eshell/prompt-pwd))
          (propertize (pkg-eshell/-current-git-branch)
                      'face 'pkg-eshell/prompt-git-branch)
          (propertize " λ" 'face (if (zerop eshell-last-command-status) 'success 'error))
          " "))

;;;;; Autoloads

;;;###autoload
(defun pkg-eshell/clear ()
  (interactive)
  (eshell/clear-scrollback)
  (eshell-emit-prompt))

;;;###autoload
(defun pkg-eshell/copy-input ()
  (interactive)
  (if (pkg-eshell/-prompt-p)
      (kill-new (eshell-get-old-input))
    (save-excursion
      (eshell-previous-prompt 1)
      (kill-new (eshell-get-old-input)))))

;;;###autoload
(defun pkg-eshell/copy-output ()
  (interactive)
  (kill-new (buffer-substring-no-properties (eshell-beginning-of-output)
                                            (eshell-end-of-output))))

;;;###autoload
(defun pkg-eshell/copy-dwim ()
  (interactive)
  (if (pkg-eshell/-prompt-p)
      (pkg-eshell/copy-input)
    (pkg-eshell/copy-output)))

;;;###autoload
(defun pkg-eshell/change-line ()
  (interactive)
  (if (pkg-eshell/-prompt-p)
      (progn
        (eshell-bol)
        (evil-change-line (point) (line-end-position)))
    (evil-change-whole-line)))

;;;; Init

(add-hook 'eshell-post-command-hook #'pkg-eshell/rename-buffer-with-last-input)

(setq eshell-banner-message "")
(setq eshell-directory-name (concat my/cache-dir "eshell/"))
(setq eshell-kill-processes-on-exit t)
(setq eshell-scroll-to-bottom-on-input 'all)
(setq eshell-scroll-to-bottom-on-output 'all)
(setq eshell-history-size 10000)

;; If non-nil, term buffers are destroyed after their processes die. WARNING:
;; Setting this to non-nil may result in unexpected behavior for short-lived
;; processes, see bug#18108.
(setq eshell-destroy-buffer-when-process-dies nil)

;; Uncomment to remove all escape sequences. Default is nil.
;; (setq eshell-preoutput-filter-functions '(ansi-color-filter-apply))
(setq eshell-preoutput-filter-functions nil)

(setq eshell-output-filter-functions
      '(eshell-postoutput-scroll-to-bottom
        eshell-handle-control-codes
        eshell-handle-ansi-color
        eshell-watch-for-password-prompt))

;; Example, use Eshell's sudo.
(setq eshell-prefer-lisp-functions t
      eshell-prefer-lisp-variables t)

(setq eshell-save-history-on-exit t)
(setq eshell-hist-ignoredups t)

(setq eshell-modules-list
      '(eshell-alias
        ;; eshell-banner
        eshell-basic
        eshell-cmpl
        eshell-dirs
        eshell-glob
        eshell-hist
        eshell-ls
        eshell-pred
        eshell-prompt
        eshell-script
        eshell-term
        ;; Required to have eshell/sudo available, which in turn supports
        ;; password caching. Unfortunately, eshell/sudo doesn't work properly.
        ;; For example, it blocks Emacs while the sudo command is running.
        ;;
        ;; eshell-tramp
        eshell-unix))

;; Do not add input beginning with empty space to history.
(setq eshell-input-filter #'eshell-input-filter-initial-space)

(setq eshell-glob-case-insensitive t)

;; Visual commands require a proper terminal. Eshell can't handle that, so it
;; delegates these commands to a term buffer.
(setq eshell-visual-commands '("bat"
                               "htop"
                               "less"
                               "more"
                               "ncmpcpp"
                               "screen"
                               "tig"
                               "tmux"
                               "top"
                               "vi"
                               "vim"))

;; For exa colored output, consider the answer from
;; https://emacs.stackexchange.com/questions/51027/missing-color-support-for-exa-in-eshell.
(setq eshell-command-aliases-list
      '(("q"  "exit")
        ("f"  "find-file $1")
        ("d"  "dired $1")
        ("l" "exa -lg --sort=name --group-directories-first --time-style long-iso --git")
        ("ll" "exa -lag --sort=name --group-directories-first --time-style long-iso --git $*")
        ("git" "git --no-pager $*")
        ("gs" "magit-status")
        ("clear" "clear-scrollback")))

(setq eshell-prompt-regexp "^.* λ ")
(setq eshell-prompt-function #'pkg-eshell/default-prompt)

;; It is an error for a glob pattern not to match. This mimics the behavior of
;; zsh if non-nil, but bash if nil.
(setq eshell-error-if-no-glob t)

;;;; Config

(add-to-list 'display-buffer-alist
             `(,(rx line-start "*eshell" (zero-or-more not-newline) line-end)
               (display-buffer-in-side-window)
               (window-width . 0.5)
               (side . right)
               (slot . 0)
               (body-function . select-window)))

;;; File place persistence

(defun pkg-saveplace/reposition ()
  "Force windows to recenter current line (with saved position)."
  (run-with-timer
   0 nil
   (lambda (buf)
     (when (buffer-live-p buf)
       (dolist (win (get-buffer-window-list buf nil t))
         (with-selected-window win (recenter)))))
   (current-buffer)))

(add-hook 'elpaca-after-init-hook #'save-place-mode)
(add-hook 'find-file-hook 'pkg-saveplace/reposition t)

(setq save-place-file (concat my/cache-dir "saveplace"))
(setq save-place-limit 100)

;;; Goto address
;;;; Commentary

;; Activate URLs and e-mail addresses. When this buffer-local minor mode is
;; enabled, it finds all the URLs in the buffer, highlights them, and turns them
;; into clickable buttons.

;;;; Init

(add-hook 'text-mode-hook #'goto-address-mode)
(add-hook 'prog-mode-hook #'goto-address-prog-mode)

;; NOTE: There's no keybinding because I prefer to use `embark-act' followed
;; by RET.

;;; Highlight line
;;;; Variables

(defvar-local pkg-hl-line/mode-on nil
  "Store the hl-line-mode state for the current buffer.

Disable `hl-line-mode' when selecting text because the selection
is already visible.")

;;;; Private

(defun pkg-hl-line/on-maybe ()
  (when pkg-hl-line/mode-on
    (hl-line-mode +1)))

(defun pkg-hl-line/off-maybe ()
  (when pkg-hl-line/mode-on
    (hl-line-mode -1)))

(defun pkg-hl-line/update-mode-state ()
  (when hl-line-mode
    (setq pkg-hl-line/mode-on t)))

;;;; Init

(add-hook 'dired-mode-hook #'hl-line-mode)
(add-hook 'prog-mode-hook #'hl-line-mode)
(add-hook 'text-mode-hook #'hl-line-mode)
(add-hook 'conf-mode-hook #'hl-line-mode)
(add-hook 'org-agenda-mode-hook #'hl-line-mode)
(add-hook 'tablist--mode-hook #'hl-line-mode)

(add-hook 'evil-visual-state-entry-hook #'pkg-hl-line/off-maybe)
(add-hook 'activate-mark-hook #'pkg-hl-line/off-maybe)

(add-hook 'evil-visual-state-exit-hook #'pkg-hl-line/on-maybe)
(add-hook 'deactivate-mark-hook #'pkg-hl-line/on-maybe)

(add-hook 'hl-line-mode-hook #'pkg-hl-line/update-mode-state)

;; Not having to render the hl-line overlay in multiple buffers offers a tiny
;; performance boost. I also don't need to see it in other buffers.
(setq hl-line-sticky-flag nil
      global-hl-line-sticky-flag nil)

;; Emacs 28
(setq hl-line-overlay-priority -50)

;;; Image dired

(setq image-dired-thumb-size 150
      image-dired-thumb-margin 2
      ;; No border around thumbnails
      image-dired-thumb-relief 0)

;; Where to store image caches.
(setq image-dired-dir (concat my/cache-dir "image-dired/")
      image-dired-db-file (concat image-dired-dir "db.el")
      image-dired-gallery-dir (concat image-dired-dir "gallery/")
      image-dired-temp-image-file (concat image-dired-dir "temp-image")
      image-dired-temp-rotate-image-file (concat image-dired-dir "temp-rotate-image"))

;;; Isearch

;; Isearch has been improved in Emacs 27 and can now display match numbers in
;; the modeline.
;;
;; Highlights matches in the full buffer. It is useful in combination with
;; `lazy-highlight-cleanup' customized to nil to leave matches highlighted in
;; the whole buffer after exiting isearch.
(setq isearch-lazy-count t
      isearch-allow-scroll t
      lazy-highlight-initial-delay 0
      lazy-highlight-buffer t
      lazy-highlight-cleanup t
      lazy-highlight-buffer-max-at-a-time 10)

;;; Ispell

(when-let ((dir (getenv "ASPELL_DICT_DIR")))
  (setq ispell-extra-args `("--dict-dir" ,dir)))
(add-to-list 'ispell-skip-region-alist '(":\\(PROPERTIES\\|LOGBOOK\\):" . ":END:"))
(add-to-list 'ispell-skip-region-alist '("#\\+BEGIN_SRC" . "#\\+END_SRC"))

;;; Mail

;; Specially needed for Gmail.
(setq mail-host-address "gmail.com")

(setq user-full-name "Icaro Motta")
(setq user-mail-address "icaro.ldm@gmail.com")

;; Setup User-Agent header. It is set to `message-user-agent' which means we
;; will be using `message-mode' to compose mail.
(setq mail-user-agent 'message-user-agent)

;; Disable warnings about changes in `mail-user-agent'.
(setq compose-mail-user-agent-warnings nil)

;;;; Message

(setq message-sendmail-envelope-from 'header)

;; In case we want to sendmail with a different account than the default one.
;; (setq message-sendmail-extra-arguments '("-a" "..."))
;; (setq message-send-mail-function #'sendmail-send-it)

;; Non-nil means that the message buffer will be killed after sending a message.
(setq message-kill-buffer-on-exit t)

;; For Debians, set to nil. Non-nil means don't add "-f username" to the
;; sendmail command line.
(setq message-sendmail-f-is-evil nil)

;;;; Sendmail

;; If non-nil, specify the envelope-from address when sending mail. The value
;; used to specify it is whatever is found in the variable mail-envelope-from,
;; with user-mail-address as fallback.
(setq mail-specify-envelope-from t)

;; TODO: use msmtpq to queue sending mail.
(setq sendmail-program (executable-find "msmtp"))
(setq mail-specify-envelope-from t)
(setq mail-envelope-from 'header)

;;; Minibuffer history

(add-hook 'elpaca-after-init-hook #'savehist-mode)

(setq savehist-file (concat my/cache-dir "savehist"))
(setq savehist-save-minibuffer-history t)

;; Save on kill only.
(setq savehist-autosave-interval nil)

;; We don't need to add minibuffer history variables to this list.
(setq savehist-additional-variables '(search-ring regexp-search-ring))

;;; Outline
;;;; Private

(defun pkg-outline/recenter (&rest _args)
  (recenter))

;;;; Outloads

;;;###autoload
(defun pkg-outline/delete-tree ()
  "Delete tree at point."
  (interactive)
  (let ((start nil)
        (end nil))
    (save-excursion
      (outline-back-to-heading)
      (setq start (point))
      (outline-end-of-subtree)
      (setq end (point)))
    (delete-region start end)
    (delete-region (line-beginning-position) (1+ (line-end-position)))))

;;;###autoload
(defun pkg-outline/next-hide-other-heading ()
  (interactive)
  (call-interactively #'outline-next-heading)
  (call-interactively #'outline-hide-other))

;;;###autoload
(defun pkg-outline/previous-hide-other-heading ()
  (interactive)
  (call-interactively #'outline-previous-heading)
  (call-interactively #'outline-hide-other))

;;;; Init

(defun pkg-outline/config ())

(setq outline-minor-mode-highlight 'override)

;; Because C-i is translated as <TAB>, when the cursor is on an outline, it'll
;; toggle the subtree's visibility instead of `evil-jump-forward'.
(setq outline-minor-mode-cycle t)

;;;; Config

(advice-add #'outline-backward-same-level :after #'pkg-outline/recenter)
(advice-add #'outline-forward-same-level :after #'pkg-outline/recenter)
(advice-add #'outline-next-visible-heading :after #'pkg-outline/recenter)
(advice-add #'outline-previous-visible-heading :after #'pkg-outline/recenter)
(advice-add #'outline-cycle-buffer :after #'pkg-outline/recenter)
(advice-add #'outline-hide-other :after #'pkg-outline/recenter)

;;; Proced
;;;; Variables

(defconst pkg-proced/proced-keywords
  `((,(concat "^\s+\\(.*?\\)\s+\\(.*?\\)\s+\\(.*?\\)\s+\\(.*?\\)\s+"
       "\\(.*?\\)\s+\\(.*?\\)\s+\\(.*\\)")
     (1 'pkg-proced/user)
     (2 'pkg-proced/pid)
     (3 'pkg-proced/cpu)
     (4 'pkg-proced/mem)
     (5 'pkg-proced/time-start)
     (6 'pkg-proced/time-duration)
     (7 'pkg-proced/process)))
  "Extra font-lock patterns for the `proced' menu.")

;;;; Faces

(defface pkg-proced/user '((t :inherit shadow))
  "Face for user indicator in `proced'."
  :group 'process)

(defface pkg-proced/pid
  '((((class color) (min-colors 88) (background light))
     :foreground "#5317ac")
    (((class color) (min-colors 88) (background dark))
     :foreground "#b6a0ff"))
  "Face for PID indicator in `proced'."
  :group 'process)

(defface pkg-proced/cpu
  '((((class color) (min-colors 88) (background light))
     :foreground "#8f0075")
    (((class color) (min-colors 88) (background dark))
     :foreground "#f78fe7"))
  "Face for memory indicator in `proced'."
  :group 'process)

(defface pkg-proced/mem
  '((((class color) (min-colors 88) (background light))
     :foreground "#0031a9")
    (((class color) (min-colors 88) (background dark))
     :foreground "#2fafff"))
  "Face for CPU indicator in `proced'."
  :group 'process)

(defface pkg-proced/time-start
  '((((class color) (min-colors 88) (background light))
     :foreground "#30517f")
    (((class color) (min-colors 88) (background dark))
     :foreground "#a0bfdf"))
  "Face for start time indicator in `proced'."
  :group 'process)

(defface pkg-proced/time-duration
  '((((class color) (min-colors 88) (background light))
     :foreground "#00538b")
    (((class color) (min-colors 88) (background dark))
     :foreground "#00cdc8"))
  "Face for time indicator in `proced'."
  :group 'process)

(defface pkg-proced/process nil
  "Face for process indicator in `proced'."
  :group 'process)

;;;; Autoloads

;;;###autoload
(define-minor-mode pkg-proced/extra-keywords
  "Apply extra font-lock rules to diff buffers."
  :init-value nil
  :global t
  (if pkg-proced/extra-keywords
      (progn
        (font-lock-flush (point-min) (point-max))
        (font-lock-add-keywords nil pkg-proced/proced-keywords nil)
        (add-hook 'proced-mode-hook #'pkg-proced/extra-keywords))
    (font-lock-remove-keywords nil pkg-proced/proced-keywords)
    (remove-hook 'proced-mode-hook #'pkg-proced/extra-keywords)
    (font-lock-flush (point-min) (point-max))))

;;;; Init

(add-hook 'proced-mode-hook #'pkg-proced/extra-keywords)

(setq-default proced-auto-update-flag t)
(setq proced-auto-update-interval 2)
(setq proced-descend t)
(setq proced-filter 'user)

;;; Programming languages, tree-sitter

;; Maximum amount of syntax highlighting. Level 3 is too limited for JSON.
(setq-default treesit-font-lock-level 4)

;; Some .so files can be downloaded/built from
;; https://github.com/casouri/tree-sitter-module/releases.
(setq treesit-extra-load-path
      (list (concat my/local-dir "tree-sitter-grammars/")))

;;;; Javascript

(defun pkg-js-mode/setup-sibling-rules ()
  (setq-local find-sibling-rules
              (list
               ;; Source -> unit test
               (list (rx (group (+ (not "/")))
                         "." (group (or "js" "jsx" "ts" "tsx"))
                         string-end)
                     (rx (regex "\\1")
                         ".test." (regex "\\2")
                         string-end))

               ;; Unit test -> source
               (list (rx (group (+ (not "/")))
                         ".test." (group (or "js" "jsx" "ts" "tsx"))
                         string-end)
                     (rx (regex "\\1")
                         "." (regex "\\2")
                         string-end))

               ;; Source -> spec test
               (list (rx (group (+ (not "/")))
                         "." (group (or "js" "jsx" "ts" "tsx"))
                         string-end)
                     (rx (regex "\\1")
                         ".spec." (regex "\\2")
                         string-end))

               ;; Spec test -> source
               (list (rx (group (+ (not "/")))
                         ".spec." (group (or "js" "jsx" "ts" "tsx"))
                         string-end)
                     (rx (regex "\\1")
                         "." (regex "\\2")
                         string-end)))))

(add-hook 'js-mode-hook #'js-ts-mode)
(add-hook 'js-mode-hook #'pkg-js-mode/setup-sibling-rules)
(add-hook 'typescript-tsx-mode-hook #'pkg-js-mode/setup-sibling-rules)

;;;; CSS

(add-to-list 'auto-mode-alist
             (cons (rx "." (or "css") string-end)
                   'css-ts-mode))

;;;; JSON

(add-to-list 'auto-mode-alist
             (cons (rx "." (or "json") string-end)
                   'json-ts-mode))

;;;; XML

(add-to-list 'auto-mode-alist
             (cons (rx "." (or "pom" "plist" "rss" "xml" "xsd" "xslt") string-end)
                   'nxml-mode))

;; Typing a slash automatically completes the end-tag.
(setq nxml-slash-auto-complete-flag t)
(setq nxml-auto-insert-xml-declaration-flag nil)

;;; Project

(defun pkg-project/locate-project (dir)
  (let ((override (locate-dominating-file dir ".project.el")))
    (if override
        (cons 'vc override)
      nil)))

;;;; Init

(add-hook 'project-find-functions #'project-try-vc)
(add-hook 'project-find-functions #'pkg-project/locate-project)

(setq project-list-file (concat my/cache-dir "projects"))
(setq project-switch-commands #'project-dired)

;;; Recent files

(defun pkg-recentf/add-dired-directory ()
  "Add dired directory to recentf file list."
  (recentf-add-file default-directory))

(defun pkg-recentf/file-truename (file)
  (if (or (file-remote-p file nil t)
          (not (file-remote-p file)))
      (file-truename file)
    file))

(add-hook 'kill-emacs-hook #'recentf-cleanup)
(add-hook 'dired-mode-hook #'pkg-recentf/add-dired-directory)
(add-hook 'elpaca-after-init-hook #'recentf-mode)

(setq recentf-save-file (concat my/cache-dir "recentf")
      recentf-auto-cleanup 'never
      recentf-max-menu-items 0
      recentf-max-saved-items 100)

;; Functions to post process recent file names. They are successively passed a
;; file name to transform it.
(setq recentf-filename-handlers
      '(substring-no-properties
        pkg-recentf/file-truename
        abbreviate-file-name))

;;; Security, encryption
;;;; EPA

;; Disable auto-saving when opening an encrypted file.
(setq epa-file-inhibit-auto-save t)

;; Do not cache passphrase for symmetric encryption.
(setq epa-file-cache-passphrase-for-symmetric-encryption nil)

;;;; EPG

;; With gpg version 2.1+ we can avoid the GUI pinentry and enter the passphrase
;; directly in Emacs.
(setq epg-pinentry-mode 'loopback)

;;;; Password cache

(setq password-cache t)

;; How many seconds passwords are cached.
(setq password-cache-expiry 60)

;;; Show parentheses

;; By default, there’s a small delay before showing a matching parenthesis. It
;; can be deactivated with the following (which you have to do before activating
;; show-paren-mode).
(setq show-paren-delay 0
      show-paren-highlight-openparen t
      show-paren-when-point-inside-paren t
      show-paren-when-point-in-periphery t

      ;; This new setting in Emacs 29.1 doesn't work well with Evil.
      show-paren-context-when-offscreen nil)

(add-hook 'prog-mode-hook #'show-paren-mode)

;;; Snippets
;;;; Commentary

;; Tempo is a powerful snippet expansion system. For examples and documentation:
;;
;; - https://www.emacswiki.org/emacs/TempoSnippets
;; - http://www.lysator.liu.se/~davidk/elisp/tempo-examples.html

;;;; Variables

(defvar-local pkg-tempo/tags-emacs-lisp nil)

;;;; Private

(defun pkg-tempo/setup-emacs-lisp ()
  (tempo-define-template
   "emacs-lisp-defun"
   '("(defun " p " (" p ")" n>
     "\"" p "\""
     n> r ")")
   "defun"
   "Insert a defun expression."
   'pkg-tempo/tags-emacs-lisp)

  (tempo-use-tag-list 'pkg-tempo/tags-emacs-lisp))

(defun pkg-tempo/setup ()
  (pkg-tempo/setup-emacs-lisp)

  ;; Catch-all default regex.
  (setq-default tempo-match-finder
                (rx line-start
                    (group (one-or-more not-newline))
                    point)))

;;;; Init

(add-hook 'emacs-lisp-mode-hook #'pkg-tempo/setup)

;;; Time

;; Display time, load level, and mail flag in mode lines.

(setq display-time-format "%H:%M"
      display-time-interval 60
      display-time-mail-directory nil
      display-time-default-load-average nil)

;;; Timeclock

(setq timeclock-file (concat my/cache-dir "timelog"))

;;; Tramp

;; Value of TERM environment variable for logging in to remote host.
(setq tramp-terminal-type "tramp")

;; Set from 0 to 11 (more verbose). Level 1 means only errors will be logged.
;; Default is 3.
(setq tramp-verbose 1)

;; Set path to file which keeps connection history.
(setq tramp-persistency-file-name (concat my/cache-dir "tramp"))

;;; Transient

(setq transient-values-file (concat my/cache-dir "transient/values.el")
      transient-levels-file (concat my/cache-dir "transient/levels.el")
      transient-history-file (concat my/cache-dir "transient/history.el"))

;; If non-nil, then the key binding of each suffix is colorized to indicate
;; whether it exits the transient state or not.
;;
;; Note: The colors look odd in most themes.
(setq transient-semantic-coloring nil)

;;; URL

(setq url-configuration-directory (concat my/cache-dir "url/"))

;;; Winner
;;;; Init

(setq winner-dont-bind-my-keys t)

(setq winner-boring-buffers
      '("*Apropos*"
        "*Buffer List*"
        "*Compile-Log*"
        "*Fuzzy Completions*"
        "*Help*"
        "*Ibuffer*"
        "*cvs*"
        "*esh command on file*"
        "*inferior-lisp*"))

;;;; Config

(winner-mode +1)
;;; Daemon
;;;; Private

(defun pkg-daemon/-after-make-frame-functions-h (frame)
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

(defun pkg-daemon/-inhibit-emacsclient-message ()
  "Inhibit initial emacsclient frame message.

Remove initial message 'When done with this frame type C-x 5 0'
when starting with 'emacsclient --create-frame' (the original
solution is in
https://emacs.stackexchange.com/questions/44883/remove-emacsclient-startup-message)."
  (setq inhibit-message t)
  (run-with-idle-timer 0 nil (lambda () (setq inhibit-message nil))))

;;;; Init

(add-hook 'server-after-make-frame-hook #'pkg-daemon/-inhibit-emacsclient-message)
(add-hook 'after-make-frame-functions #'pkg-daemon/-after-make-frame-functions-h)

;;; Theme
;;;; Commentary

;; Alternating between a list of favorite themes and persisting that choice
;; should be straightforward, for instance to choose a light theme during the
;; morning. Emacs doesn't have a concept of /current theme/, so it doesn't
;; ship with any solution to persist and reload the user's choice. The solution
;; below does the simplest possible thing, which is to store the preference in a
;; custom variable which is persisted at ~/.emacs.d/.local/cache/custom.el.
;;
;; Tmux colours were taken from https://github.com/mattdavis90/base16-tmux and
;;
;; Kitty themes: https://github.com/kovidgoyal/kitty-themes/tree/master/themes

;;;; Private

(defun pkg-theme/dark-p ()
  (equal 'dark (frame-parameter nil 'background-mode)))

(defun pkg-theme/write (theme)
  (with-temp-file "~/.theme"
    (erase-buffer)
    (insert (format "SYSTEM_THEME=%s\n" theme))
    (insert (format "THEME_BG_COLOR=%s\n" (face-attribute 'default :background)))))

(defun pkg-theme/update-kitty (theme)
  "Switch Kitty THEME and update all running instances."
  (make-directory "~/.config/kitty" 'parents)
  (let ((kitty-theme (cond ((equal theme 'doom-one) '("Doom_One" . "Doom One"))
                           ((equal theme 'doom-one-light) '("Doom_One_Light" . "Doom One Light"))
                           (:default '("Doom_One" . "Doom One")))))
    (with-temp-file "~/.config/kitty/current-theme.conf"
      (insert "include themes/" (car kitty-theme) ".conf"))))

(defun pkg-theme/update-tmux (theme)
  "Globally change all Tmux windows based on symbol THEME."
  (let* ((script (expand-file-name "~/data/repos/dotfiles/tmux/theme.sh"))
         (cmd (format "%s '%s'" script theme)))
    (start-process-shell-command "tmux" nil cmd)))

(defun pkg-theme/update-system-macos (theme-mode)
  (let ((dark-enabled (if (pkg-theme/dark-p) "true" "false")))
    (start-process-shell-command "osascript" nil
                                 (format "osascript -l JavaScript -e \"Application('System Events').appearancePreferences.darkMode = %s\"" dark-enabled))))

(defun pkg-theme/update-system-gtk (theme-mode)
  (let ((theme (if (pkg-theme/dark-p) my/gtk-dark-theme my/gtk-light-theme)))
    (start-process-shell-command "gsettings" nil
                                 (format "gsettings set org.gnome.desktop.interface gtk-theme %s" theme))))

(defun pkg-theme/update-system-kde (theme-mode)
  ;; Disabled because this has a side-effect of resetting other
  ;; configurations, like the KDE task switcher mode.
  ;;
  ;; (let ((theme (if (pkg-theme/dark-p) my/kde-dark-theme my/kde-light-theme)))
  ;;   (start-process-shell-command "lookandfeeltool" nil
  ;;                                (format "lookandfeeltool --apply %s" theme)))
  )

(defun pkg-theme/gtk? ()
  (equal "GNOME" (getenv "XDG_CURRENT_DESKTOP")))

(defun pkg-theme/kde? ()
  (equal "KDE" (getenv "XDG_CURRENT_DESKTOP")))

(defun pkg-theme/update-system (theme-mode)
  "Change system theme according to THEME-MODE."
  (cond ((pkg-theme/gtk?)
         (pkg-theme/update-system-gtk theme-mode))
        ((pkg-theme/kde?)
         (pkg-theme/update-system-kde theme-mode))
        (my/mac?
         (pkg-theme/update-system-macos theme-mode))))

(defun pkg-theme/merge-face-attributes (face attributes)
  "Make a face spec suitable for `custom-theme-set-faces'.

ATTRIBUTES are appended to the face spec in order to replace
attributes currently used by FACE."
  (let ((face-attributes (face-all-attributes face (selected-frame))))
    `(,face ((t ,@(seq-mapcat (lambda (e) (list (car e) (cdr e))) face-attributes)
              ,@attributes)))))

(defun pkg-theme/customize-common (theme)
  "Update faces with common customizations.

This function should be used to set face attributes that don't
change between dark/light themes and it should be called 'after'a
theme is loaded in order to correctly update all faces."
  (custom-theme-set-faces
   theme
   (pkg-theme/merge-face-attributes 'variable-pitch `(:family ,my/face-variable-pitch-family))
   (pkg-theme/merge-face-attributes 'fixed-pitch `(:family ,my/face-fixed-pitch-family))
   (pkg-theme/merge-face-attributes 'fixed-pitch-serif `(:family ,my/face-fixed-pitch-family))

   (when (pkg-theme/dark-p)
     (pkg-theme/merge-face-attributes 'cursor `(:background "coral1")))

   (cond ((equal my/theme 'doom-one-light)
          (pkg-theme/merge-face-attributes 'shadow `(:foreground "gray65")))))

  (with-eval-after-load 'ctrlf
    (custom-theme-set-faces
     theme
     (pkg-theme/merge-face-attributes 'ctrlf-highlight-active '(:inherit dired-marked))))

  (with-eval-after-load 'org-modern
    (custom-theme-set-faces
     theme
     ;; The weight must be set to bold, otherwise the checkbox under the cursor
     ;; becomes smaller.
     (pkg-theme/merge-face-attributes 'org-modern-symbol '(:weight bold))))

  (with-eval-after-load 'ledger-mode
    (custom-theme-set-faces
     theme
     ;; Disable highlight face (inherited by default).
     (pkg-theme/merge-face-attributes 'ledger-occur-xact-face '(:inherit nil))))

  (with-eval-after-load 'org
    (custom-theme-set-faces
     theme
     (pkg-theme/merge-face-attributes 'org-block '(:inherit fixed-pitch))
     (pkg-theme/merge-face-attributes 'org-table '(:inherit fixed-pitch))
     (pkg-theme/merge-face-attributes 'org-formula '(:inherit fixed-pitch))
     (pkg-theme/merge-face-attributes 'org-verbatim '(:inherit fixed-pitch))
     (pkg-theme/merge-face-attributes 'org-special-keyword '(:inherit fixed-pitch))
     (pkg-theme/merge-face-attributes 'org-meta-line '(:inherit fixed-pitch))
     (pkg-theme/merge-face-attributes 'org-todo '(:inherit fixed-pitch))
     (pkg-theme/merge-face-attributes 'org-document-title '(:height 1.6))))

  (with-eval-after-load 'dirvish
    (custom-theme-set-faces
     theme
     (pkg-theme/merge-face-attributes 'dirvish-hl-line '(:inherit 'hl-line))))

  (with-eval-after-load 'notmuch
    (custom-theme-set-faces
     theme
     (pkg-theme/merge-face-attributes 'notmuch-search-unread-face '(:weight bold))))

  (with-eval-after-load 'anzu
    (custom-theme-set-faces
     theme
     (pkg-theme/merge-face-attributes 'anzu-replace-highlight '(:inherit (query-replace bold) :strike-through t))
     (pkg-theme/merge-face-attributes 'anzu-replace-to '(:strike-through nil)))))

(defun pkg-theme/update-default-frame-bg-color ()
  (let ((default-bg (face-attribute 'default :background)))
    (setf (cdr (assoc 'background-color default-frame-alist))
          (if (equal "unspecified-bg" default-bg)
              my/default-dark-bg
            default-bg))))

(defun pkg-theme/load-advice (original-fn theme &optional no-confirm no-enable)
  (mapc #'disable-theme (remq theme custom-enabled-themes))
  (when (funcall original-fn theme 'no-confirm no-enable)
    (if (pkg-theme/dark-p)
        (pkg-theme/update-system 'dark)
      (pkg-theme/update-system 'light))
    (setq my/theme theme)
    (pkg-theme/write theme)
    (unless my/windows? (pkg-theme/update-kitty theme))
    (pkg-theme/update-tmux theme)
    (pkg-theme/customize-common theme)
    (pkg-theme/update-default-frame-bg-color)
    (when (fboundp 'org-modern--update-label-face)
      (org-modern--update-label-face))))

(defun pkg-theme/load ()
  (condition-case err
      (load-theme my/theme 'no-confirm)
    (error
     (message "Warning: Could not load theme '%s'. Error '%s'. " my/theme err)
     (unless (equal my/theme 'modus-operandi)
       (message "Loading default theme.")
       (advice-remove 'load-theme #'pkg-theme/load-advice)
       (load-theme 'modus-operandi 'no-confirm)))))

;;;; Autoloads

;;;###autoload
(defun pkg-theme/cycle-dark-light ()
  "Alternate between dark and light themes."
  (interactive)
  (cond ((pkg-theme/dark-p)
         (load-theme my/favorite-light-theme 'no-confirm))
        (:default
         (load-theme my/favorite-dark-theme 'no-confirm))))

;;;###autoload
(defun pkg-theme/cycle ()
  "Cycle over themes."
  (interactive)
  (let ((current-theme (car my/favorite-themes))
        (next-theme (cadr my/favorite-themes))
        ;; We need to define a new binding because the advised `load-theme'
        ;; function will save a custom variable and this has the side-effect of
        ;; resetting `my/favorite-themes' to what's persisted on custom.el.
        (favorite-themes my/favorite-themes))
    (load-theme next-theme 'no-confirm)
    (customize-save-variable 'my/favorite-themes
                             (append (cdr favorite-themes)
                                     (list (car favorite-themes))))))

;;;; Config

(setq modus-themes-mode-line '(accented borderless))
(setq modus-themes-region '(bg-only))
(setq modus-themes-completions 'opinionated)
(setq modus-themes-bold-constructs t)
(setq modus-themes-italic-constructs nil)
(setq modus-themes-paren-match '(bold intense))
(setq modus-themes-syntax '(alt-syntax))
(setq modus-themes-org-blocks 'tinted-background)
(setq modus-themes-scale-headings t)

(add-hook 'elpaca-after-init-hook #'pkg-theme/load)

;; Advice after the theme is first loaded because `pkg-theme/load-advice' calls
;; `customize-save-variable' which is pretty slow.
(advice-add 'load-theme :around #'pkg-theme/load-advice)
