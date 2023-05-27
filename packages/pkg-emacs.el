;;; -*- lexical-binding: t; -*-

;;; Private

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

;;; Autoloads

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

(my/package emacs
  :elpaca nil

  :hook (emacs-lisp-mode-hook . cl-font-lock-built-in-mode)

  ;; Although this checker is very useful for certain kinds of projects
  ;; (e.g. package maintainers), I prefer to disable the checkdoc
  ;; because it's extremely annoying most of the time. I don't really
  ;; care about adding a 'Commentary' section to every elisp file, for
  ;; example.
  :hook (emacs-lisp-mode-hook . outline-minor-mode)
  :hook (outline-minor-mode-hook . pkg-elisp-mode/outline-minor-mode-h)

  :init
  (setq battery-mode-line-limit 99
        battery-update-interval 60
        battery-load-low 20
        battery-load-critical 10)

  :config
  (advice-add #'calculate-lisp-indent :override #'pkg-elisp-mode/calculate-lisp-indent))

(provide 'pkg-emacs)
