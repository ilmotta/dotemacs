;;; -*- lexical-binding: t; -*-

;;; Custom

(defcustom pkg-cider/app-refresh-form
  "(user/refresh)"
  "Clojure form used to refresh the application."
  :type 'string
  :group 'cider)

(make-variable-buffer-local 'pkg-cider/app-refresh-form)

;;; Variables

(defvar pkg-cider/last-test-var nil
  "Last test var executed.")

(defvar pkg-cider/last-test-ns nil
  "Last test namespace executed.")

;;; Private

(defun pkg-cider/-as-test-var (var)
  (if (string-prefix-p ":" var) ; It's a keyword
      (concat (thread-last
                (substring var 1)
                (replace-regexp-in-string (rx (or ":" ".")) "-")
                (replace-regexp-in-string (rx "/") "-"))
              "-test")
    keyword))

(defun pkg-cider/-cljs-execute-test-var (var)
  (let ((var (pkg-cider/-as-test-var var)))
    (cider-interactive-eval
     (concat "(require '[cljs-run-test])"
             (format "(cljs-run-test/run-test %s)" var)))))

(defun pkg-cider/-cljs-execute-test-ns (ns)
  (cider-interactive-eval
   (if ns
       (format "(cljs.test/run-tests (quote %s))" ns)
     "(cljs.test/run-tests)")))

(defun pkg-cider/setup-clojure-h ()
  "Disable certain LSP features when `cider-mode' is enabled."
  ;; There are multiple issues related to CIDER completion functions. I could
  ;; only get a working and performant auto-completion when `completion-styles'
  ;; is set to its default value.
  ;;
  ;; https://github.com/clojure-emacs/cider/issues/3006
  ;; https://github.com/clojure-emacs/cider/issues/3019
  ;; https://github.com/minad/corfu/issues/8
  (setq-local completion-styles '(basic partial-completion emacs22))

  (when (bound-and-true-p lsp-mode)
    ;; Prefer CIDER completion when enabled.
    (if (bound-and-true-p cider-mode)
        (setq-local lsp-enable-completion-at-point nil)
      (setq-local lsp-enable-completion-at-point t))))

(defun pkg-cider/-update-last-test (ns var)
  "Update the last test by setting NS and VAR."
  (setq pkg-cider/last-test-ns ns
        pkg-cider/last-test-var var))

(defun pkg-cider/hide-trailing-whitespace ()
  (setq show-trailing-whitespace nil))

(defun pkg-cider/special-form-clojure (form inspect-fn-name)
  (concat "(let [res " form "]"
          "  (cond"
          "    (instance? clojure.lang.IDeref res)"
          "    (deref res)"

          "    :else res"
          "  ))"))

(defun pkg-cider/special-form-clojurescript (form inspect-fn-name)
  (concat "(let [res    " form "]"
          "  (cond"
          "    (satisfies? IDeref res)"
          "    (" inspect-fn-name "(deref res))"

          "    (.-then res)"
          "    (-> res"
          "        (.then #(" inspect-fn-name " {:ok %}))"
          "        (.catch #(" inspect-fn-name " {:error %})))"

          "    :else res"
          "  ))"))

;;; Autoloads

;;;###autoload
(defun pkg-cider/run-test ()
  "Run the test at point."
  (interactive)
  (if (derived-mode-p #'clojurescript-mode)
      (progn
        (cider-eval-defun-at-point)
        (let* ((ns (clojure-find-ns))
               (def (clojure-find-def))
               (deftype (car def))
               (var (cadr def)))
          (if (and ns (string-match-p (rx "deftest" (* not-newline))
                                      deftype))
              (progn
                (pkg-cider/-update-last-test ns var)
                (pkg-cider/-cljs-execute-test-var var))
            (user-error "No test at point"))))
    (call-interactively #'cider-test-run-test)))

;;;###autoload
(defun pkg-cider/run-ns-tests ()
  (interactive)
  (if (derived-mode-p #'clojurescript-mode)
      (progn
        (setq pkg-cider/last-test-var nil)
        (pkg-cider/-cljs-execute-test-ns nil))
    (call-interactively #'cider-test-run-ns-tests)))

;;;###autoload
(defun pkg-cider/rerun-tests ()
  (interactive)
  (if (derived-mode-p #'clojurescript-mode)
      (progn
        (cond ((and pkg-cider/last-test-ns pkg-cider/last-test-var)
               (pkg-cider/-cljs-execute-test-var pkg-cider/last-test-var))
              (pkg-cider/last-test-ns
               (pkg-cider/-cljs-execute-test-ns pkg-cider/last-test-ns))
              (t
               (user-error "No test to re-run"))))
    (call-interactively #'cider-test-rerun-test)))

;;;###autoload
(defun pkg-cider/run-project-tests ()
  (interactive)
  (when (not (derived-mode-p #'clojurescript-mode))
    (call-interactively #'cider-test-run-project-tests)))

;;;###autoload
(defun pkg-cider/reframe-inspect-db ()
  (interactive)
  (cider-nrepl-sync-request:eval
   "(tap> @re-frame.db/app-db)"))

;;;###autoload
(defun pkg-cider/portal-open ()
  (interactive)
  (let ((ns (if (derived-mode-p 'clojurescript-mode)
                "portal.web"
              "portal.api")))
    (cider-nrepl-sync-request:eval
     (concat (format "(require '[%s :as portal])" ns)
             "(portal/tap)"
             "(portal/open)"))))

;;;###autoload
(defun pkg-cider/portal-clear ()
  (interactive)
  (let ((ns (if (derived-mode-p 'clojurescript-mode)
                "portal.web"
              "portal.api")))
    (cider-nrepl-sync-request:eval
     (concat (format "(require '[%s :as portal])" ns)
             "(portal/clear)"))))

;;;###autoload
(defun pkg-cider/repl-clear-buffer ()
  (interactive)
  (with-current-buffer (cider-current-repl nil 'ensure)
    (cider-repl-clear-buffer)))

;;;###autoload
(defun pkg-cider/load-all-project-files ()
  "Load (eval) all Clojure(Script) files. `cider-load-all-project-ns' doesn't work
as expected."
  (interactive)
  (cider-load-all-files (project-root (project-current))))

;;;###autoload
(defun pkg-cider/eval-special-form (inspect-fn-name)
  "Eval form around point as a promise and resolves/rejects with
INSPECT-FN-NAME, otherwise eval as usual."
  (interactive)
  (let ((bounds (evil-cp--get-enclosing-bounds t)))
    (if (not bounds)
        (error "No surrounding form found.")
      (let* ((form (buffer-substring-no-properties (car bounds) (cdr bounds)))
             (around-form (cond ((derived-mode-p 'clojurescript-mode)
                                 (pkg-cider/special-form-clojurescript form inspect-fn-name))
                                ((derived-mode-p 'clojure-mode)
                                 (pkg-cider/special-form-clojure form inspect-fn-name)))))
        (cider-interactive-eval around-form)))))

;;;###autoload
(defun pkg-cider/eval-defun-2nd-symbol ()
  "Eval 2nd symbol starting from the beginning of defun.

This is particularly useful to evaluate the value of a var."
  (interactive)
  (save-excursion
    (beginning-of-defun)
    (forward-symbol 2)
    (call-interactively #'cider-eval-last-sexp)))

;;;###autoload
(defun pkg-cider/eval-special-form-tap ()
  "Eval form around point as a promise and resolves/rejects with
`tap>', otherwise eval as usual."
  (interactive)
  (pkg-cider/eval-special-form "tap>"))

;;;###autoload
(defun pkg-cider/eval-special-form-print ()
  "Eval form around point as a promise and resolves/rejects with
`println', otherwise eval as usual."
  (interactive)
  (pkg-cider/eval-special-form "println"))

;;;###autoload
(defun pkg-cider/app-start ()
  (interactive)
  (cider-interactive-eval "(user/start)"))

;;;###autoload
(defun pkg-cider/app-refresh ()
  (interactive)
  (cider-interactive-eval pkg-cider/app-refresh-form))

;;;###autoload
(defun pkg-cider/app-stop ()
  (interactive)
  (cider-interactive-eval "(user/stop)"))

;;;###autoload
(defun pkg-cider/shadow-watch-compile ()
  "Trigger a recompile for all running builds.

This command is specially useful in Shadow-CLJS projects that
have {:devtools {:autobuild false}}. This configuration is often
desirable in cases where hot-reloading is not reliable and/or is
too expensive. Some people save files multiple times per minute,
for example."
  (interactive)
  (cider-map-repls :clj
    (lambda (connection)
      (let ((form "(shadow.cljs.devtools.api/watch-compile-all!)"))
        (cider--prep-interactive-eval form connection)
        (cider-nrepl-request:eval form
                                  (lambda (_response))
                                  (cider-current-ns)
                                  nil
                                  nil
                                  nil
                                  connection)))))

;;;###autoload
(defun pkg-cider/kill-repls ()
  "Kill without confirmation all CIDER REPL buffers."
  (interactive)
  (thread-last (buffer-list)
               (seq-filter (lambda (buf)
                             (string-prefix-p "*cider-repl " (buffer-name buf))))
               (seq-each (lambda (buf)
                           (let ((kill-buffer-query-functions nil))
                             (kill-buffer buf))))))

(defun pkg-cider/repl-buffer-p (buffer &optional _arg)
  (with-current-buffer buffer
    (derived-mode-p 'cider-repl-mode)))

;;; Package

;; CIDER - The Clojure Interactive Development Environment that Rocks.
(my/package
  (cider :ref "17743001467e0045ecd6639aad45d21e89d6b9a2")
  :defer t
  :commands (cider
             cider-connect
             cider-jack-in
             cider-version
             cider-view-manual)

  :mode (("\\.clj\\'" . clojure-mode)
         ("\\.cljs\\'" . clojurescript-mode))

  ;; Subword minor mode is useful for Java/Javascript interop.
  :hook (cider-repl-mode-hook . subword-mode)
  :hook (cider-repl-mode-hook . smartparens-mode)

  ;; Disable annoying trailing whitespace warnings.
  :hook (cider-repl-mode-hook . pkg-cider/hide-trailing-whitespace)
  :hook (cider-test-report-mode-hook . pkg-cider/hide-trailing-whitespace)

  :hook (cider-mode-hook . pkg-cider/setup-clojure-h)

;;;; Keybindings
  :init
  (my/general-mode-def
    :keymaps '(clojurescript-mode-map)
    "SPC" #'pkg-cider/shadow-watch-compile)

  (my/general-mode-def
    :keymaps '(clojure-mode-map cider-repl-mode-map)
    ;; Application
    "a s" #'pkg-cider/app-start
    "a x" #'pkg-cider/app-stop
    "a r" #'pkg-cider/app-refresh
    ;; Debug
    "d f"   #'cider-debug-defun-at-point
    "d p o" #'pkg-cider/portal-open
    "d p c" #'pkg-cider/portal-clear
    ;; Eval
    "e :"   #'cider-read-and-eval
    "e F"   #'pkg-cider/eval-special-form-tap
    "e b"   #'cider-eval-buffer
    "e e"   #'cider-eval-defun-at-point
    "e f"   #'cider-eval-list-at-point
    "e i"   #'cider-inspect-last-sexp
    "e l"   #'cider-eval-last-sexp
    "e n"   #'cider-eval-ns-form
    "e p f" #'cider-pprint-eval-defun-at-point
    "e p F" #'pkg-cider/eval-special-form-print
    "e p p" #'cider-pprint-eval-last-sexp
    "e r"   #'cider-eval-region
    "e u"   #'cider-undef
    "e 2"   #'pkg-cider/eval-defun-2nd-symbol
    ;; Help
    "h ."   #'cider-doc
    "h a a" #'cider-apropos
    "h a d" #'cider-apropos-documentation
    "h c"   #'cider-clojuredocs
    "h d"   #'devdocs-lookup-clojure
    "h j"   #'cider-javadoc
    "h w"   #'cider-clojuredocs-web
    ;; Inspect
    "i r"   #'cider-inspect-last-result
    "i d"   #'pkg-cider/reframe-inspect-db
    ;; Macroexpand
    "m m"   #'cider-macroexpand-1
    "m M"   #'cider-macroexpand-all
    ;; Namespace
    "n L"   #'pkg-cider/load-all-project-files
    "n f"   #'cider-find-ns
    "n l l" #'cider-ns-reload
    "n l a" #'cider-ns-reload-all
    "n b b" #'cider-browse-ns
    "n b a" #'cider-browse-ns-all
    "n r"   #'cider-ns-refresh
    ;; REPL
    "r R"   #'cider-restart
    "r b"   #'cider-switch-to-repl-buffer
    "r x"   #'cider-quit
    "r X"   #'pkg-cider/kill-repls
    "r c"   #'pkg-cider/repl-clear-buffer
    "r s"   #'cider-jack-in
    ;; Tests
    "t ."   #'pkg-cider/run-test
    "t b"   #'pkg-cider/run-ns-tests
    "t l"   #'pkg-cider/rerun-tests
    "t p"   #'pkg-cider/run-project-tests)

  ;; The CIDER welcome message obscures error messages that the above code is
  ;; supposed to be make visible.
  (setq cider-repl-display-help-banner nil)

  (setq cider-auto-select-error-buffer nil
        cider-auto-select-test-report-buffer nil
        cider-show-error-buffer 'except-in-repl)

  (setq cider-repl-history-display-duplicates nil
        cider-repl-history-display-style 'one-line
        cider-repl-history-file (concat my/cache-dir "cider-repl-history")
        cider-repl-history-highlight-current-entry t
        cider-repl-history-highlight-inserted-item t
        cider-repl-history-maximum-display-length 100
        cider-repl-history-quit-action 'delete-and-restore
        cider-repl-history-size 1000
        cider-repl-result-prefix ";; => "
        cider-repl-wrap-history t)

  ;; Do not open CIDER buffer after successfull
  ;; connection to REPL.
  (setq cider-repl-pop-to-buffer-on-connect nil)

  ;; Although I visually prefer to see debug keys in the minibuffer, it doesn't
  ;; work properly, e.g. eldoc replaces them.
  (setq cider-debug-prompt 'overlay)

  ;; TODO: fix keybindings for evil.
  ;;
  ;; Evil-collection does support keybindings for CIDER debug, but it doesn't
  ;; work for me.
  ;;
  ;; (setq cider-debug-prompt-commands ...)

  ;; Do not save the file on load. Eval is one thing, saving a file is another
  ;; with potential side-effects. This is specially important when developing
  ;; with hot reload (e.g. Shadow CLJS). ELisp also doesn't save the file when
  ;; `eval-buffer' is called.
  (setq cider-save-file-on-load nil)

  ;; Hide the *nrepl-connection* and *nrepl-server* buffers from
  ;; appearing in some buffer switching commands like
  ;; switch-to-buffer. Useful for debugging Cider
  ;; (setq nrepl-hide-special-buffers t)

  ;; Do not prompt for symbol confirmation.
  ;; E.g. show docs and jump to definition without
  ;; asking.
  (setq cider-prompt-for-symbol nil)

  ;; When you evaluate code in Clojure files, the result is
  ;; displayed in the buffer itself, in an overlay right after
  ;; the evaluated code. If you want this overlay to be
  ;; font-locked (syntax-highlighted) like Clojure code
  (setq cider-overlays-use-font-lock t)

  ;; CIDER can colorize usages of functions and
  ;; variables from any namespace, not only macros and
  ;; core Clojure functions.
  (setq cider-font-lock-dynamically '(macro core function var deprecated))

  ;; Prefer to set these per project using directory local variables.
  (comment
    (setq cider-jack-in-dependencies '(("nrepl/nrepl" "0.9.0")))

    (when (executable-find "zprint")
      (setq cider-print-fn 'zprint)))

  ;; Use Fast Idiomatic Pretty Printer (5-10x faster
  ;; than clojure.core/pprint)
  ;;
  ;; See https://docs.cider.mx/cider/usage/pretty_printing.html
  (setq cider-repl-use-pretty-printing t)

  ;; This is extremely useful for debug purposes when enabled.
  (setq nrepl-log-messages nil)

  :config
  (with-eval-after-load 'evil
    ;; Add to jump list, i.e. record location prior to running commands.
    (evil-add-command-properties #'cider-test-run-project-tests :jump t))

  (add-to-list 'display-buffer-alist
               `(pkg-cider/repl-buffer-p
                 (display-buffer-in-side-window)
                 (window-width . 0.33)
                 (side . right)
                 (slot . 0)
                 (window-parameters . ((no-delete-other-windows . t))))))

(provide 'pkg-cider)
