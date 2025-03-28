;;; -*- lexical-binding: t; -*-

;;; Code:

(require 'benchmark)
(require 'my-globals)
(require 'lib-elisp)
(require 'map)

;;; Other macros

(defmacro lib-util/with-buf-reuse-window (&rest body)
  "Force buffer switch/pop to reuse the same window whenever
possible, but fallback `display-buffer-same-window'."
  `(let ((display-buffer-overriding-action '((display-buffer-reuse-window
                                              display-buffer-same-window))))
    ,@body))

(defmacro lib-util/benchmark (id &rest body)
  `(push (list ,id (benchmark-elapse ,@body))
    my/start-up-benchmarks))

(defmacro lib-util/require (&rest packages)
  (declare (indent nil))
  `(progn
     ,@(cl-loop for pkg in packages
        collect `(lib-util/benchmark (quote ,pkg) (require (quote ,pkg))))
     nil))

(defmacro lib-util/pkg (order &rest args)
  (declare (indent defun))
  (when my/use-package-force-demand
    (setq args (map-delete args :defer))
    (setq args (map-delete args :demand))
    (setq args (append '(:demand t) args)))
  `(use-package ,order ,@args))

;;; Interactive fundamentals

(defun lib-util/advice-remove-dwim ()
  (interactive)
  (let ((advised-fn (intern (thing-at-point 'symbol 'no-properties))))
    (if-let ((fns (advice-alist advised-fn)))
        (let ((choice  (intern (completing-read "Advice to remove: "
                                                (map-keys fns)))))
          (advice-remove advised-fn choice))
      (message "No advice found."))))

;;; Process

(defun lib-util/call-process (cmd)
  "Execute CMD synchronously.

Returns (STATUS . OUTPUT) when it is done, where STATUS is the returned error
code of the process and OUTPUT is its stdout output."
  (with-temp-buffer
    (cons (or (call-process-shell-command cmd nil t nil)
              -1)
          (string-trim (buffer-string)))))

(defun lib-util/async-shell-command-no-window (command)
  "Execute string COMMAND asynchronously without opening buffer."
  (interactive "sAsync shell command: ")
  (let* ((buffer-name "*Async Shell Command*")
         (output-buffer (get-buffer-create buffer-name))
         (process (let ((display-buffer-alist (list (list buffer-name #'display-buffer-no-window))))
                    (async-shell-command command output-buffer)
                    (get-buffer-process output-buffer)))
         (sentinel (lambda (process signal)
                     (when (memq (process-status process) '(exit signal))
                       (shell-command-sentinel process signal)
                       ;; Here you could run arbitrary code when the
                       ;; command is successful.
                       ;; (when (zerop (process-exit-status process))
                       ;;   (message "%s" ,cmd))
                       ))))
    (when (process-live-p process)
      (set-process-sentinel process sentinel))))

;;; UI

(defun lib-util/toggle-background-transparency ()
  "Toggle background transparency."
  (interactive)
  (let ((alpha-transparency 93))
    (set-frame-parameter nil 'alpha-background
                         (if (eq alpha-transparency (frame-parameter nil 'alpha-background))
                             100
                           alpha-transparency))))

;;; Utilities

(defun lib-util/native-compile-my-emacs ()
  (interactive)
  (native-compile-async (list (file-name-concat user-emacs-directory "core")
                              (file-name-concat user-emacs-directory "lisp")
                              (file-name-concat user-emacs-directory "packages"))))

(defun lib-util/increment-integer-at-point ()
  "Increment integer at point.
Does not understand negative integers."
  (interactive)
  (save-excursion
    (if (looking-at "[0-9]+")
        (replace-match (number-to-string (1+ (string-to-number (match-string 0)))))
      (error "No number at point"))))

(cl-defun lib-util/memoize-ttl (&key f (ttl-ms 10000))
  (let ((cached nil)
        (called-at nil))
    (lambda (&rest args)
      (if (and called-at (<= (- (* 1000 (ts-unix (ts-now)))
                                (* 1000 (ts-unix called-at)))
                             ttl-ms))
          cached
        (setq cached (apply f args))
        (setq called-at (ts-now))
        cached))))

(defun lib-util/uuid ()
  "Generate UUID."
  (cdr (lib-util/call-process "uuidgen")))

;;;###autoload
(defun lib-util/buffer-new (&optional force-new-p)
  "Create new empty buffer. New buffer will be named *untitled*,
*untitled*<2>, etc."
  (interactive "P")
  (let ((buf-name (if force-new-p (generate-new-buffer "*untitled*")
                    "*untitled*")))
    (switch-to-buffer buf-name)
    (when force-new-p
      (funcall initial-major-mode))
    (with-current-buffer buf-name
      (setq-local buffer-offer-save t))
    (get-buffer buf-name)))

;;;###autoload
(defun lib-util/clone-buffer-dwim ()
  "Clone buffer even if it's a file visiting buffer."
  (interactive)
  (if buffer-file-name
      (let ((original-buffer-file-name buffer-file-name)
            (original-buffer (current-buffer)))
        (setq-local buffer-file-name nil)
        (ignore-errors (clone-buffer nil 'pop))
        (with-current-buffer original-buffer
          (setq-local buffer-file-name original-buffer-file-name)))
    (call-interactively #'clone-buffer)))

;;;###autoload
(defun lib-util/find-user-init-file ()
  "Edit the `user-init-file', in another window."
  (interactive)
  (find-file (concat user-emacs-directory "init.el")))

;;;###autoload
(defun lib-util/find-user-custom-file ()
  "Edit the `user-init-file', in another window."
  (interactive)
  (find-file (concat my/cache-dir "custom.el")))

(defun lib-util/advice-once (symbol where function &optional props)
  "Add advice, but remove it after calling FUNCTION.

One example usage is when configuring packages. You might want to
enable a package's mode before calling SYMBOL, thus allowing you
do defer loading it until it's actually needed."
  (advice-add symbol :after (lambda (&rest _) (advice-remove symbol function)))
  (advice-add symbol where function props))

(defun lib-util/add-hook-once (hook fn &optional depth local)
  "Add HOOK, but remove it after first call to FN. DEPTH and LOCAL
are passed directly to `add-hook'."
  (cl-labels ((wrapper ()
                (remove-hook hook #'wrapper local)
                (funcall fn)))
    (add-hook hook #'wrapper depth local)))

(defun lib-util/message-error (err)
  (error "%s" (propertize (format "Error: %s" err) 'face 'compilation-error)))

;;;###autoload
(defun lib-util/advice-unadvice (sym)
  "Remove all advices from symbol SYM."
  (interactive "aFunction symbol: ")
  (advice-mapc (lambda (advice _props) (advice-remove sym advice)) sym))

(defun lib-util/mute-echo-area (original-fn &rest args)
  "Temporarily disable logging in the echo area.
This function should be used in :around advices."
  (let ((inhibit-message t))
    (ignore-errors (apply original-fn args))))

(defun lib-util/inhibit-message (original-fn &rest args)
  "Temporarily inhibit messages in the echo area.
Use this function in an :around advice."
  (let ((inhibit-message t))
    (apply original-fn args)))

(defun lib-util/org-fill-paragraphs ()
  "Fill paragraphs in the active region, line by line.

`forward-paragraph` doesn't work as expected, so we move line by
line instead."
  (interactive)
  (save-excursion
    (save-restriction
      (widen)
      (let ((beginning (region-beginning))
            (end (region-end)))
        (goto-char beginning)
        (while (and (<= (point) end) (not (eobp)))
          (org-fill-paragraph nil nil)
          (forward-line))))))

(defun lib-util/--org-unfill-paragraph-p ()
  "Returns true when the paragraph should be unfilled.
Note that by default `org-fill-paragraph' will unfill paragraphs
within comment blocks."
  (interactive)
  (and (not (looking-at-p (rx (zero-or-more space) line-end)))
       (not (member (car (org-element-at-point))
                    '(comment
                       comment-block
                       example-block
                       headline
                       keyword
                       node-property
                       property-drawer
                       quote-block
                       src-block)))))

(defun lib-util/sort-up-sexp ()
  "Rudimentary solution to sort s-exps. The cursor must be on a
valid position so that the surrouding s-exp contains all the
lines that should be sorted. S-exps in the lines to be sorted
must not have line breaks, otherwise pairs will be unbalanced."
  (interactive)
  (let ((beg nil)
        (end nil)
        (aggressive-indent-mode-p nil))

    (when (bound-and-true-p aggressive-indent-mode)
      (let ((inhibit-message t))
        (setq aggressive-indent-mode-p +1)
        (aggressive-indent-mode -1)))

    ;; Break line on opening s-exp and save point.
    (paredit-backward-up)
    (forward-char)
    (newline)
    (beginning-of-line)
    (setq beg (point))

    ;; Break line on closing s-exp and save point.
    (paredit-forward-up)
    (forward-char -1)
    (newline)
    (forward-line -1)
    (end-of-line)
    (setq end (point))

    ;; The Emacs sort functions are affected by whitespace.
    (indent-region beg end)

    ;; Reset the points after fixing indentation.
    (save-excursion
      (goto-char beg)
      (beginning-of-line)
      (setq beg (point)))
    (save-excursion
      (goto-char end)
      (end-of-line)
      (setq end (point)))

    ;; Sort and join lines.
    (sort-lines nil beg end)

    (when aggressive-indent-mode-p
      (let ((inhibit-message t))
        (aggressive-indent-mode +1)))

    (join-line 'next)
    (paredit-backward-up)
    (join-line 'next)))

(defun lib-util/sudo-file-path (file)
  (let ((host (or (file-remote-p file 'host) "localhost")))
    (concat "/" (when (file-remote-p file)
                  (concat (file-remote-p file 'method) ":"
                          (if-let (user (file-remote-p file 'user))
                              (concat user "@" host)
                            host)
                          "|"))
            "sudo:root@" host
            ":" (or (file-remote-p file 'localname)
                    file))))

(defun lib-util/toggle-buffer (buffer-or-name cmd)
  "Either bury/kill BUFFER-OR-NAME when visible or run CMD
interactively."
  (if (get-buffer-window buffer-or-name)
      (save-selected-window
        (with-current-buffer buffer-or-name
          (if buffer-file-name
              (bury-buffer)
            (kill-buffer))))
    (call-interactively cmd)))

;;;###autoload
(defun lib-util/unfill-dwim (&optional start end)
  (interactive)
  (let ((fill-column most-positive-fixnum))
    (cond ((and start end)
           (fill-region start end))

          (start
           (fill-region start (point-max)))

          ((use-region-p)
           (fill-region (region-beginning) (region-end)))

          (:default
           (fill-paragraph)))))

;;;###autoload
(defun lib-util/kill-other-buffers ()
  "Kill all buffers except the current one and non-file buffers."
  (interactive)
  (mapc 'kill-buffer
        (delq (current-buffer)
              (cl-remove-if (lambda (buffer)
                              (string-match-p "^\*.*\*$" (buffer-name buffer)))
                            (buffer-list)))))

;;;###autoload
(defun lib-util/org-babel-remove-result-buffer ()
  "Remove results from every code block in buffer."
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (while (re-search-forward org-babel-src-block-regexp nil t)
      (org-babel-remove-result))))

;;;###autoload
(defun lib-util/align-regexp (string-or-regex start end)
  "Align columns by whitespace followed by custom STRING_OR_REGEX."
  (interactive "sRegex: \nr")
  (align-regexp start end
                (concat "\\(\\s-*\\)\\s-" string-or-regex) 1 0 t))

;;;###autoload
(defun lib-util/describe-face-at-point ()
  "Show details about face at point."
  (interactive)
  (what-cursor-position t))

(defalias 'lib-util/describe-cursor-at-point #'lib-util/describe-face-at-point)

;;;###autoload
(defun lib-util/smart-beginning-of-line ()
  "Move point to the first non-whitespace character on this line.

If the command is repeated then cycle position between
`beginning-of-line' and `back-to-indentation'."
  (interactive "^")
  (if (eq last-command 'lib-util/smart-beginning-of-line)
      (if (= (line-beginning-position) (point))
          (back-to-indentation)
        (beginning-of-line))
    (back-to-indentation)))

;;;###autoload
(defun lib-util/screenshot ()
  "Save a screenshot of the current frame.

Emacs should be built with Cairo support (Emacs 28 will use it by
default) otherwise the function `x-export-frames' is not
defined."
  (interactive)
  (let ((filename (format "~/Downloads/emacs-screenshot-%s.svg"
                          (format-time-string "%Y-%m-%dT%T"))))
    (with-temp-file filename
      (insert (x-export-frames nil 'svg)))
    (kill-new filename)
    (message "Screenshot saved at %s" filename)))

;;;###autoload
(defun lib-util/process-environment ()
  "List and sort all env vars inherited by subprocesses."
  (interactive)
  (with-current-buffer (get-buffer-create "*Environment Variables*")
    (read-only-mode -1)
    (erase-buffer)
    (shell-script-mode)
    (goto-char (point-min))
    (save-excursion
      (dolist (env (seq-sort-by #'identity #'string< process-environment))
        (insert env "\n")))
    (read-only-mode +1)
    (pop-to-buffer (current-buffer))))

;;;###autoload
(defun lib-util/symlink (target link &optional replace)
  "Make a symbolic link to TARGET, named LINK.

When REPLACE is non-nil it replaces the existing LINK. When LINK
is a directory and a symbolic link, it is first removed.

TARGET is always resolved to its true name."
  (interactive (list (read-file-name "Target: " nil nil t)
                     (read-file-name "Link: " nil nil 'confirm)
                     current-prefix-arg))
  (let ((target (file-truename target))
        (link (expand-file-name link)))
    (cond
     ;; Avoid recursive links.
     ((and replace (file-symlink-p link) (file-directory-p link))
      (delete-file link 'trash)
      (make-symbolic-link target link))

     ((and (not replace)
           (not (file-symlink-p link)))
      (make-symbolic-link target link replace)))))

;;;###autoload
(defun lib-util/symlink-dotfiles (&optional replace)
  "Create symbolic links for all configuration files.

When called with prefix argument it replaces all existing
symbolic links."
  (interactive "P")
  (let ((target-root "~/data/repos/dotfiles/")
        (link-root "~/")
        (replace (if replace 'replace nil)))
    (make-directory (concat link-root ".gnupg/") 'parents)
    (lib-util/symlink (concat target-root "gpg-agent.conf")
                      (concat link-root ".gnupg/gpg-agent.conf") replace)

    ;; Ruby
    (lib-util/symlink (concat target-root "ruby/gemrc")
                      (concat link-root ".gemrc") replace)
    (lib-util/symlink (concat target-root "ruby/irbrc")
                      (concat link-root ".irbrc") replace)
    (lib-util/symlink (concat target-root "ruby/pryrc")
                      (concat link-root ".pryrc") replace)

    ;; Node.js
    (lib-util/symlink (concat target-root "nodejs/npmrc")
                      (concat link-root ".npmrc") replace)

    ;; Shell and terminal
    (lib-util/symlink (concat target-root "bashrc")
                      (concat link-root ".bashrc") replace)
    (lib-util/symlink (concat target-root "inputrc")
                      (concat link-root ".inputrc") replace)
    (lib-util/symlink (concat target-root "tmux/tmux.conf")
                      (concat link-root ".tmux.conf") replace)
    (lib-util/symlink (concat target-root "shell/zpreztorc")
                      (concat link-root ".zpreztorc") replace)
    (lib-util/symlink (concat target-root "shell/zshrc")
                      (concat link-root ".zshrc") replace)

    ;; Vim
    (make-directory (concat link-root ".vim/after/syntax/") 'parents)
    (make-directory (concat link-root ".vim/colors/") 'parents)
    (lib-util/symlink (concat target-root "vim/colors/zenburn.vim")
                      (concat link-root ".vim/colors/zenburn.vim") replace)
    (lib-util/symlink (concat target-root "vim/vimrc")
                      (concat link-root ".vimrc") replace)

    ;; Clojure
    (make-directory (concat link-root ".lein/") 'parents)
    (lib-util/symlink (concat target-root "clojure/profiles.clj")
                      (concat link-root ".lein/profiles.clj") replace)
    (make-directory (concat link-root ".clojure/") 'parents)
    ;; Unfortunately Clojure deps does not work with symbolic links.
    (copy-file (concat target-root "clojure/deps.edn")
               (concat link-root ".clojure/deps.edn")
               replace)

    ;; Kitty
    (make-directory (concat link-root ".config/kitty/") 'parents)
    (lib-util/symlink  (concat target-root "kitty/kitty.conf")
                       (concat link-root ".config/kitty/kitty.conf") replace)
    (lib-util/symlink (concat target-root "kitty/themes/")
                      (concat link-root ".config/kitty/themes") replace)

    ;; Misc
    (make-directory (concat link-root ".config/qutebrowser/") 'parents)
    (lib-util/symlink (concat target-root "qutebrowser-config.py")
                      (concat link-root ".config/qutebrowser/config.py") replace)

    (make-directory (concat link-root ".gnupg/") 'parents)
    (lib-util/symlink (concat target-root "gpg-agent.conf")
                      (concat link-root ".gnupg/gpg-agent.conf") replace)

    (lib-util/symlink (concat target-root "aspell.conf")
                      (concat link-root ".aspell.conf") replace)
    (lib-util/symlink (concat target-root "editorconfig")
                      (concat link-root ".editorconfig") replace)
    (lib-util/symlink (concat target-root "git/gitconfig")
                      (concat link-root ".gitconfig") replace)
    (lib-util/symlink (concat target-root "git/gitignore")
                      (concat link-root ".gitignore") replace)

    (lib-util/symlink "~/data/repos/dotemacs/"
                      (concat link-root ".config/emacs") replace)
    (lib-util/symlink "~/data/repos/dotemacs/"
                      (concat link-root ".emacs.d") replace)

    ;; Enable if reading e-mail in Emacs.
    ;; (make-directory (concat link-root ".config/afew/") 'parents)
    ;; (make-directory (concat link-root ".mail/.notmuch/hooks") 'parents)
    ;; (lib-util/symlink (concat target-root "mail/notmuch/hooks/post-new")
    ;;                  (concat link-root ".mail/.notmuch/hooks/post-new") replace)
    ;; (lib-util/symlink (concat target-root "mail/notmuch/hooks/pre-new")
    ;;                  (concat link-root ".mail/.notmuch/hooks/pre-new") replace)
    ))

;;;###autoload
(defun lib-util/profile-function (fn)
  "Profile FN and display the report buffer."
  (interactive "aProfile function: ")
  (profiler-start 'cpu)
  (call-interactively fn)
  (profiler-report)
  (profiler-stop)
  (select-window (previous-window)))

;;;###autoload
(defun lib-util/project-switch-to-dotfiles ()
  "Open my dotfiles project."
  (interactive)
  (lib-util/with-buf-reuse-window
   (project-switch-project "~/data/repos/dotfiles")))

;;;###autoload
(defun lib-util/sudo-find-file (file)
  "Open FILE as root."
  (interactive "FOpen file as root: ")
  (find-file (lib-util/sudo-file-path file)))

;;;###autoload
(defun lib-util/yank-buffer-filename ()
  "Copy the current buffer's path to the kill ring."
  (interactive)
  (if-let (filename (or buffer-file-name (bound-and-true-p list-buffers-directory)))
      (message (kill-new (abbreviate-file-name filename)))
    (error "Couldn't find filename in current buffer")))

;;;###autoload
(defun lib-util/yank-buffer-absolute-path ()
  "Copy the current buffer's absolute path to the kill ring."
  (interactive)
  (if-let (filename (or buffer-file-name (bound-and-true-p list-buffers-directory)))
      (message (kill-new (expand-file-name filename)))
    (error "Couldn't find filename in current buffer")))

;;;###autoload
(defun lib-util/yank-buffer-absolute-dir-path ()
  "Copy the absolute buffer directory to the kill ring."
  (interactive)
  (if-let (filename (or buffer-file-name (bound-and-true-p list-buffers-directory)))
      (message (kill-new (f-dirname (expand-file-name filename))))
    (error "Couldn't find filename in current buffer")))

;;;###autoload
(defun lib-util/profiler-start ()
  "Stop current CPU profiler and start it again."
  (interactive)
  (profiler-cpu-stop)
  (profiler-start 'cpu))

;;;###autoload
(defun lib-util/profiler-stop ()
  "Automatically display the report when finished."
  (interactive)
  (profiler-report)
  (profiler-stop)
  (select-window (previous-window)))

;;;###autoload
(defun lib-util/init-find-early-init-file ()
  (interactive)
  (find-file (concat user-emacs-directory "early-init.el")))

;;;###autoload
(defun lib-util/start-up-report ()
  (interactive)
  (let* ((buf (get-buffer-create "*start-up-report*"))
         (factor 1000)
         (total (* factor (seq-reduce (lambda (sum benchmark)
                                        (+ sum (cadr benchmark)))
                                      my/start-up-benchmarks 0)))
         (benchmarks (append
                      '(hline)
                      '(("ID" "Time (ms)" "Remaining (ms)" "Remaining (%)"))
                      '(hline)
                      (thread-last my/start-up-benchmarks
                                   (seq-sort-by #'cadr #'>=)
                                   (seq-map-indexed (lambda (benchmark index)
                                                      (let ((val (* factor (cadr benchmark)))
                                                            (remaining (seq-reduce (lambda (sum b)
                                                                                     (+ sum (* factor (cadr b))))
                                                                                   (seq-subseq my/start-up-benchmarks index)
                                                                                   0)))
                                                        (list (car benchmark)
                                                              (format "%.0f" val)
                                                              (format "%.0f" remaining)
                                                              (format "%.0f" (* 100 (/ remaining total)))
                                                              (format "%.2f" (* 100 (/ val total))))))))
                      '(hline))))
    (with-current-buffer buf
      (goto-char (point-min))
      (erase-buffer)
      (org-mode)
      (insert (orgtbl-to-table.el benchmarks nil) "|")
      (goto-char (point-min))
      (insert "#+title: Start-up Report\n\n")
      (pop-to-buffer buf))))

;;;###autoload
(defun lib-util/font-preview (text)
  "Preview all available font families."
  (interactive "sText to preview: ")
  (let ((buffer-name "*font families*"))
    (with-current-buffer (get-buffer-create buffer-name)
      (erase-buffer)
      (dolist (font-family (seq-sort #'string< (font-family-list)))
        (insert (propertize (concat font-family ": " text) 'face `(:family ,font-family :height 1.4))
                "\n"))
      (goto-char (point-min)))
    (pop-to-buffer-same-window buffer-name)))

(defun lib-util/shell-escape-single-quote (file)
  "Escape single quote in FILE for shell."
  (s-replace "'" "'\"'\"'" file))

(defun lib-util/listening-p (port)
  "Return t when there's a process listening on PORT."
  (with-temp-buffer
    (let* ((cmd (format "lsof -i -P -n | grep LISTEN | grep java | grep %s" port))
           (exit-code (call-process-shell-command cmd)))
      (zerop exit-code))))

(defun lib-util/process-pids (regexp)
  (interactive)
  (let* ((cmd (format "ps aux | grep %s | grep -v grep" regexp))
         (out (with-temp-buffer
                (call-process-shell-command cmd nil (current-buffer))
                (buffer-string))))
    (thread-last (split-string out "\n")
                 (seq-map
                  (lambda (line)
                    (nth 1 (seq-remove #'string-empty-p (split-string line " ")))))
                 (seq-filter #'identity))))

(defun lib-util/promise-each (actions)
  "Asynchronously process each action.

Every action in ACTIONS is a function that takes a single
argument with the value returned by the previous promise. Each
function must return a promise."
  (seq-reduce (lambda (previous action)
                (promise-then previous (lambda (value) (funcall action value))))
              actions
              (promise-resolve nil)))

(defun lib-util/promise-serialize (immediate)
  "Wrap promise-returning IMMEDIATE fn to wait previous promise.

Implementation based on
https://thoughtspile.github.io/2018/06/20/serialize-promises/"
  (let ((last (promise-resolve nil)))
    (lambda (&rest args)
      (setq last (thread-first last
                               (promise-catch (lambda (reason) reason))
                               (promise-then (lambda (_) (apply immediate args)))))
      last)))

(defun lib-util/promise-timer-while (secs repeat pred function)
  (promise-new
   (lambda (resolve _reject)
     (cl-labels ((fn ()
                   (if (funcall pred)
                       (progn
                         (funcall function)
                         (run-with-timer repeat nil #'fn))
                     (funcall resolve nil))))
       (if (numberp secs)
           (run-with-timer secs nil #'fn)
         (fn))))))

;; Sequentially process a map of functions
;; (comment
;;   (let* ((resolve-n (lambda (n) (promise-new (lambda (resolve _reject)
;;                                           (run-at-time
;;                                            n nil (lambda ()
;;                                                    (message "%s" n)
;;                                                    (funcall resolve n)))))))
;;          (resolve-n-serial (lib-util/promise-serialize resolve-n)))
;;     (thread-first (seq-map (lambda (n) (funcall resolve-n-serial n)) '(3 2 1))
;;       (promise-all)
;;       (promise-then (lambda (values) (message "%s" values))))))

(defun lib-util/comment-dwim ()
  "Comment/uncomment while balancing S-expressions."
  (interactive)
  (require 'evil-nerd-commenter)
  (require 'paredit)
  (let* ((curr-char (char-after))
         (lisp-mode-p (member major-mode my/lisp-modes))
         (lisp-sexp-p (or (equal curr-char ?\()
                          (equal curr-char ?\))
                          (equal curr-char ?\[)
                          (equal curr-char ?\])
                          (equal curr-char ?\{)
                          (equal curr-char ?\}))))
    (cond
     ;; Comment the whole sexp when at the beginning or end of it.
     ((and lisp-mode-p lisp-sexp-p
           (not (region-active-p))
           (not (sp-point-in-comment)))
      (save-excursion
        (let ((sexp (sp-get-sexp)))
          (set-mark (map-elt sexp :beg))
          (goto-char (map-elt sexp :end))
          (let ((comment-empty-lines t))
            (paredit-comment-dwim)))))

     ;; Uncomment the whole sexp when at the beginning or end of it.
     ((and lisp-mode-p lisp-sexp-p
           (not (region-active-p))
           (sp-point-in-comment))
      (save-excursion
        (let ((sexp (sp-get-sexp)))
          (evilnc--invert-comment (map-elt sexp :beg) (map-elt sexp :end)))))

     ;; When inside a string, comment at the starting position.
     ((and lisp-mode-p
           (sp-point-in-string)
           (not (sp-point-in-comment)))
      (save-excursion
        (let ((str (sp-get-string)))
          (goto-char (map-elt str :beg))
          (sp-comment))))

     ;; When inside a symbol, comment at the starting position (considering quoted
     ;; symbols as well).
     ((and lisp-mode-p
           (sp-point-in-symbol)
           (not (sp-point-in-comment)))
      (save-excursion
        (let* ((symbol (sp-get-symbol))
               (prefix (map-elt symbol :prefix)))
          (goto-char (- (map-elt symbol :beg) (length prefix)))
          (sp-comment))))

     (:default (call-interactively #'evilnc-comment-or-uncomment-lines)))))

;;;###autoload
(defun lib-util/read-password (&rest props)
  "Search USERNAME in authinfo.gpg and return the password."
  (when-let ((fn (map-elt (car (apply #'auth-source-search props))
                          :secret)))
    (funcall fn)))

;;;###autoload
(defun lib-util/x11-remove-window-decorations ()
  (interactive)
  (message "Please, select the X window.")
  (call-process-shell-command "xprop -format _MOTIF_WM_HINTS 32c -set _MOTIF_WM_HINTS 2")
  (message nil))

;;;###autoload
(defun lib-util/x11-reset-window-decorations ()
  (interactive)
  (message "Please, select the X window.")
  (call-process-shell-command "xprop -format _MOTIF_WM_HINTS 32c -set _MOTIF_WM_HINTS '0x2, 0x0, 0x1, 0x0, 0x0'")
  (message nil))

;;;###autoload
(defun lib-util/spaces->tabs ()
  (interactive)
  (tabify (point-min) (point-max)))

;;;###autoload
(defun lib-util/tabs->spaces ()
  (interactive)
  (untabify (point-min) (point-max)))

;;;###autoload
(defun lib-util/temp-buffer-p (buf)
  "Returns non-nil if BUF is temporary."
  (equal (substring (buffer-name buf) 0 1) " "))

;;;###autoload
(defun lib-util/real-buffer-p (buffer-or-name)
  "Returns t if BUFFER-OR-NAME is a real buffer.

Copied from Doom.

A real buffer is a useful buffer; a first class citizen in Doom. Real ones
should get special treatment, because we will be spending most of our time in
them. Unreal ones should be low-profile and easy to cast aside, so we can focus
on real ones.

The exact criteria for a real buffer is:

  1. A non-nil value for the buffer-local value of the `doom-real-buffer-p'
     variable OR
  2. Any function in `doom-real-buffer-functions' returns non-nil OR
  3. None of the functions in `doom-unreal-buffer-functions' must return
     non-nil.

If BUFFER-OR-NAME is omitted or nil, the current buffer is tested."
  (or (bufferp buffer-or-name)
      (stringp buffer-or-name)
      (signal 'wrong-type-argument (list '(bufferp stringp) buffer-or-name)))
  (when-let (buf (get-buffer buffer-or-name))
    (when-let (basebuf (buffer-base-buffer buf))
      (setq buf basebuf))
    (and (buffer-live-p buf)
         (not (lib-util/temp-buffer-p buf))
         (with-current-buffer buf
           my/real-buffer-p))))

(defun lib-util/promise-delay (delay-ms f &rest args)
  "Delays execution of F for DELAY-MS. ARGS will be directly passed
to F when the timer triggers. The promise will be resolved with
the return value of F."
  (promise-new (lambda (resolve _reject)
                 (run-at-time (/ delay-ms 1000)
                              nil
                              (lambda ()
                                (funcall resolve (apply f args)))))))

(defun lib-util/throttle (interval-ms fn)
  "Return a throttled version of FN that runs at most once every INTERVAL-MS."
  (let ((last-time 0)
        (interval-s (/ interval-ms 1000)))
    (lambda (&rest args)
      (let ((now (float-time)))
        (when (> (- now last-time) interval-s)
          (setq last-time now)
          (apply fn args))))))

(defun lib-util/debounce (threshold-ms f)
  "Debounces synchronous function F within THRESHOLD-MS."
  (let* ((called-at nil)
         (timer (timer-create))
         (threshold-s (/ threshold-ms 1000))
         (delay (lambda (args)
                  (setq called-at (time-to-seconds))
                  (setq timer (run-with-timer threshold-s nil
                                              (lambda (_)
                                                (setq called-at nil)
                                                (apply f args))
                                              nil)))))
    (lambda (&rest args)
      (if called-at
          (if (< (- (time-to-seconds) called-at) threshold-s) ; within threshold
              (progn
                (cancel-timer timer)
                (funcall delay args))
            (cancel-timer timer)
            (setq called-at nil)
            (apply f args))
        (funcall delay args)))))

(defun lib-util/debounce-promise (threshold-ms f)
  "Debounces async function F within THRESHOLD-MS. F must return a promise."
  (let* ((called-at nil)
         (timer (timer-create))
         (threshold-s (/ threshold-ms 1000))
         (delay (lambda (args)
                  (promise-new (lambda (resolve reject)
                                 (setq called-at (time-to-seconds))
                                 (setq timer (run-at-time threshold-s nil
                                                          (lambda ()
                                                            (setq called-at nil)
                                                            (promise-chain (apply f args)
                                                              (then (lambda (value)
                                                                      (funcall resolve value)))
                                                              (catch (lambda (err)
                                                                       (funcall reject err))))))))))))
    (lambda (&rest args)
      (if called-at
          (if (< (- (time-to-seconds) called-at) threshold-s) ; within threshold
              (progn
                (cancel-timer timer)
                (funcall delay args))
            (cancel-timer timer)
            (setq called-at nil)
            (apply f args))
        (funcall delay args)))))

(defun lib-util/calc-reading-time (start end &optional words-per-min)
  "Calculate the estimated reading time for the selected region.
START and END define the region. Optionally, WPM (words per minute) can
be specified."
  (interactive "r\nP")
  (let* ((words-per-min (or words-per-min 225))
         (word-count (count-words start end))
         (minutes (/ word-count (float words-per-min)))
         (minutes-int (floor minutes))
         (seconds (round (* 60 (- minutes minutes-int)))))
    (message "Estimated reading time: %d minute(s) and %d second(s)" minutes-int seconds)))

;;; RCF

(comment
  (defvar lib-util/debounced-message
    (setf (symbol-function 'debounced-message)
          (lib-util/debounce 1000 #'message))
    "Debounced version of `message'.")

  (progn ; Should print once after 1s
    (debounced-message "hello world")
    (debounced-message "hello world")
    (debounced-message "hello world")
    nil)

  (defun -launch-rocket! (rocket-name)
    (lib-util/promise-delay 1000
                            #'message
                            "Launching rocket '%s'"
                            rocket-name))

  (defvar -debounced-launch-rocket!
    (setf (symbol-function '-debounced-launch-rocket!)
          (lib-util/debounce-promise 1000 #'-launch-rocket!)))

  (-debounced-launch-rocket! "Rocket A")
  (promise-all (vector (-debounced-launch-rocket! "Rocket A")
                       (-debounced-launch-rocket! "Rocket B")
                       (-debounced-launch-rocket! "Rocket C")
                       (-debounced-launch-rocket! "Rocket D")))
  ;; => print "Rocket D"
  )

(provide 'lib-util)
