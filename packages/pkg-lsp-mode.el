;;; -*- lexical-binding: t; -*-

(require 'lib-util)

(defun pkg-lsp-mode/setup-completion ()
  (setf (alist-get 'styles (alist-get 'lsp-capf completion-category-defaults))
        '(orderless)))

(defun pkg-lsp-mode/describe-thing-at-point ()
  (interactive)
  (if (cider-current-repl)
      (call-interactively #'cider-doc)
    (call-interactively #'lsp-describe-thing-at-point)))

(defun pkg-lsp-mode/setup-clojure-h ()
  "Configure local variables for `clojure-mode' only.

Also disable certain LSP features when `cider-mode' is enabled."
  ;; Not very useful because I have `aggressive-indent-mode' enabled in all
  ;; Lisp buffers.
  (setq-local lsp-enable-on-type-formatting nil)

  ;; This makes LSP very intrusive while typing.
  (setq-local lsp-enable-indentation nil)

  ;; Prefer CIDER completion when enabled.
  (if (and (fboundp 'cider-current-repl) (cider-current-repl))
      (setq-local lsp-enable-completion-at-point nil
                  lsp-completion-enable nil)
    (setq-local lsp-enable-completion-at-point t
                lsp-completion-enable t))

  (lsp-deferred))

(defun pkg-lsp-mode/disable-eldoc-h ()
  "Disable eldoc because typed signatures are too verbose, often
spanning over multiple lines."
  (setq-local lsp-eldoc-render-all nil)
  (setq-local lsp-eldoc-hook nil)
  (setq-local lsp-eldoc-enable-hover nil)
  (setq-local eldoc-documentation-strategy #'ignore))

(defun pkg-lsp-mode/lsp-mode-h ()
  "Avoid enabling LSP in certain buffers."
  (unless (member (f-filename (buffer-file-name)) '("package-lock.json"))
    (lsp-deferred)))

;; Language Server Protocol Mode (lsp-mode) aims to provide IDE-like experience
;; by providing optional integration with the most popular Emacs packages like
;; company, flycheck and project. You can enable LSP on a per-project basis
;; using /directory locals/.
;;
;;   ((js-mode . ((eval . (lsp-deferred)))))
(lib-util/pkg lsp-mode
  :elpaca (:ref "7dee0d63fa1b6628be4aaea86b2298244eb3d84e")
  :defer t

  :hook ((go-ts-mode-hook go-mode-hook) . lsp-deferred)
  :hook (lsp-completion-mode-hook . pkg-lsp-mode/setup-completion)
  :hook (typescript-mode-hook . pkg-lsp-mode/disable-eldoc-h)
  :hook ((typescript-mode-hook rjsx-mode-hook js-mode-hook) . pkg-lsp-mode/lsp-mode-h)
  :hook (clojure-mode-hook . pkg-lsp-mode/setup-clojure-h)

  :init
  ;; In the context of LSP, plists provide better performance in deserialization
  ;; and also put less presure than hash-tables. Make sure the value never
  ;; changes after compilation.
  ;;
  ;; Changing this variable will require a complete recompilation of lsp related
  ;; packages. If that's the case, make sure to remove all compiled files and
  ;; restart Emacs.
  (setq lsp-use-plists t)

  ;; The `lsp-keymap-prefix' does not work as one would think. We need to remove
  ;; the prefix keybinding and define a new one.
  (setq lsp-keymap-prefix nil)
  (general-def
    :keymaps 'lsp-mode-map
    "s-l" nil)

  ;; Common keybindings for different modes.
  (my/general-mode-def
    :keymaps '(clojure-mode-map
               go-mode-map
               go-ts-mode-map)
    "c f d" #'lsp-find-definition
    "c f i" #'lsp-find-implementation
    "c f l" #'lsp-find-declaration
    "c f r" #'lsp-find-references
    "c f R" #'lsp-treemacs-call-hierarchy
    "c f t" #'lsp-find-type-definition
    "c r"   #'lsp-rename
    "c s"   #'lsp-treemacs-symbols
    "h ."   #'pkg-lsp-mode/describe-thing-at-point)

  (setq lsp-auto-guess-root t)
  (setq lsp-session-file (concat temporary-file-directory "lsp-session"))
  (setq lsp-eslint-library-choices-file (concat my/cache-dir "lsp-eslint-choices"))
  (setq lsp-server-install-dir (concat my/cache-dir "lsp/"))

  ;; Don't offer to auto restart server.
  (setq lsp-restart 'ignore)

  ;; Common feature toggles. Some can be disabled via specific LSP language
  ;; variables, others would need to be set with buffer local variables based on
  ;; the major mode in question.
  (setq lsp-auto-configure t)
  (setq lsp-enable-dap-auto-configure t)
  (setq lsp-enable-file-watchers nil)
  (setq lsp-enable-folding nil)
  (setq lsp-enable-imenu t)
  (setq lsp-enable-indentation t)
  (setq lsp-enable-links nil)
  (setq lsp-enable-on-type-formatting nil)
  (setq lsp-enable-relative-indentation t)
  (setq lsp-enable-snippet nil)
  (setq lsp-enable-symbol-highlighting t)
  (setq lsp-enable-text-document-color t)
  (setq lsp-enable-xref t)
  (setq lsp-headerline-breadcrumb-enable nil)
  (setq lsp-lens-enable nil)
  (setq lsp-modeline-code-actions-enable nil)
  (setq lsp-modeline-diagnostics-enable nil)
  (setq lsp-modeline-workspace-status-enable nil)

  ;; LSP warns if tree sitter is being used alongside LSP for syntax
  ;; highlighting. Semantic tokens are useful, but they're a bit buggy and
  ;; slower than tree-sitter. I expect tree-sitter to be eventually supported in
  ;; all mainstream languages.
  (setq lsp-semantic-tokens-enable nil)

  ;; List of clients allowed to be used for projects. When nil, all registered
  ;; clients are considered candidates.
  (setq lsp-enabled-clients nil)

  ;; Many projects don't use ESlint, so this variable should preferably be set
  ;; in a directory locals file. The default is to globally set it as t.
  (setq lsp-eslint-enable nil)

  ;; Signatures
  (setq lsp-signature-auto-activate '(:on-trigger-char :on-server-request)
        lsp-signature-cycle t
        lsp-signature-doc-lines 1
        lsp-signature-render-documentation nil
        lsp-signature-function #'lsp-lv-message)

  ;; The default is 10s.
  (setq lsp-response-timeout 5)

  ;; Do not automatically apply suggestions before saving a file. Suggestions
  ;; are not hard rules.
  (setq lsp-before-save-edits nil)

  ;; If non-nil, log all messages to and from the language server to *lsp-log*.
  (setq lsp-log-io nil)

  ;; If you want to aggressively disable warn/info logs, set this to nil.
  ;; (setq lsp--show-message nil)

;;; Completion
  ;; LSP completion is enabled by default, but some major modes might require
  ;; setting this option to nil (e.g. CIDER already provides autocompletion).
  (setq lsp-completion-enable t)

  ;; If you need, set this to :none, otherwise lsp-mode will display an
  ;; unnecessary warning saying it could not find company-mode.
  ;;
  ;; The CAPF back-end provides a bridge to the standard
  ;; `completion-at-point-functions' facility, and thus works with any major
  ;; mode that defines a proper completion function.
  (setq lsp-completion-provider :capf)

  ;; Taken from the Corfu documentation.
  (with-eval-after-load 'corfu
    (setq lsp-completion-provider :none))

;;; Javascript configuration.

  (setq lsp-javascript-auto-closing-tags t)
  (setq lsp-javascript-implicit-project-config-check-js nil)
  (setq lsp-javascript-implicit-project-config-experimental-decorators nil)
  (setq lsp-javascript-references-code-lens-enabled nil)
  (setq lsp-javascript-suggestion-actions-enabled nil)
  (setq lsp-javascript-update-imports-on-file-move-enabled "always")
  (setq lsp-javascript-validate-enable t)
  (setq lsp-javascript-format-enable nil
        lsp-javascript-format-insert-space-after-comma-delimiter t
        lsp-javascript-format-insert-space-after-constructor nil
        lsp-javascript-format-insert-space-after-function-keyword-for-anonymous-functions t
        lsp-javascript-format-insert-space-after-keywords-in-control-flow-statements t
        lsp-javascript-format-insert-space-after-opening-and-before-closing-jsx-expression-braces nil
        lsp-javascript-format-insert-space-after-opening-and-before-closing-nonempty-braces t
        lsp-javascript-format-insert-space-after-opening-and-before-closing-nonempty-brackets nil
        lsp-javascript-format-insert-space-after-opening-and-before-closing-nonempty-parenthesis nil
        lsp-javascript-format-insert-space-after-opening-and-before-closing-template-string-braces nil
        lsp-javascript-format-insert-space-after-semicolon-in-for-statements t
        lsp-javascript-format-insert-space-before-and-after-binary-operators t
        lsp-javascript-format-insert-space-before-function-parenthesis nil
        lsp-javascript-format-place-open-brace-on-new-line-for-control-blocks nil
        lsp-javascript-format-place-open-brace-on-new-line-for-functions nil)
  (setq lsp-javascript-preferences-quote-style "single"
        lsp-javascript-preferences-import-module-specifier "auto"
        lsp-javascript-preferences-rename-shorthand-properties t)
  (setq lsp-javascript-suggest-enabled nil
        lsp-javascript-suggest-auto-imports t
        lsp-javascript-suggest-complete-function-calls nil
        lsp-javascript-suggest-complete-js-docs t
        lsp-javascript-suggest-names t
        lsp-javascript-suggest-paths t)

;;; Typescript configuration.

  ;; Specifies the folder path containing the tsserver and
  ;; lib*.d.ts files to use.
  (setq lsp-typescript-tsdk nil)
  ;; Logging can be used to diagnose TS Server issues. The log may contain file
  ;; paths, source code, and other potentially sensitive information from your
  ;; project.
  (setq lsp-typescript-tsserver-log "off")
  (setq lsp-typescript-auto-closing-tags t)
  (setq lsp-typescript-check-npm-is-installed t)
  (setq lsp-typescript-disable-automatic-type-acquisition t) ; Let each project add @types/* deps
  (setq lsp-typescript-implementations-code-lens-enabled nil)
  (setq lsp-typescript-report-style-checks-as-warnings nil)
  (setq lsp-typescript-suggestion-actions-enabled nil)
  (setq lsp-typescript-surveys-enabled nil)
  (setq lsp-typescript-tsc-auto-detect "on")
  (setq lsp-typescript-tsserver-plugin-paths nil)
  (setq lsp-typescript-tsserver-trace "off")
  (setq lsp-typescript-update-imports-on-file-move-enabled "always")
  (setq lsp-typescript-validate-enable t)
  (setq lsp-typescript-format-enable nil
        lsp-typescript-format-insert-space-after-comma-delimiter t
        lsp-typescript-format-insert-space-after-constructor nil
        lsp-typescript-format-insert-space-after-function-keyword-for-anonymous-functions t
        lsp-typescript-format-insert-space-after-keywords-in-control-flow-statements t
        lsp-typescript-format-insert-space-after-opening-and-before-closing-jsx-expression-braces nil
        lsp-typescript-format-insert-space-after-opening-and-before-closing-nonempty-braces t
        lsp-typescript-format-insert-space-after-opening-and-before-closing-nonempty-brackets nil
        lsp-typescript-format-insert-space-after-opening-and-before-closing-nonempty-parenthesis nil
        lsp-typescript-format-insert-space-after-opening-and-before-closing-template-string-braces nil
        lsp-typescript-format-insert-space-after-semicolon-in-for-statements t
        lsp-typescript-format-insert-space-after-type-assertion nil
        lsp-typescript-format-insert-space-before-and-after-binary-operators t
        lsp-typescript-format-insert-space-before-function-parenthesis nil
        lsp-typescript-format-place-open-brace-on-new-line-for-control-blocks nil
        lsp-typescript-format-place-open-brace-on-new-line-for-functions nil)
  (setq lsp-typescript-preferences-import-module-specifier "auto"
        lsp-typescript-preferences-quote-style "single"
        lsp-typescript-preferences-rename-shorthand-properties t
        lsp-typescript-references-code-lens-enabled nil)
  (setq lsp-typescript-suggest-enabled nil
        lsp-typescript-suggest-auto-imports t
        lsp-typescript-suggest-complete-function-calls nil
        lsp-typescript-suggest-complete-js-docs t
        lsp-typescript-suggest-paths t)

;;; Clojure configuration

  (setq lsp-clojure-custom-server-command "clojure-lsp")
  ;; I'll install the binary myself.
  (setq lsp-clojure-server-download-url nil)
  (setq lsp-clojure-workspace-dir (concat my/cache-dir "lsp-clojure-workspace/"))
  (setq lsp-clojure-workspace-cache-dir (concat temporary-file-directory "lsp-clojure-workspace/cache/"))

  :config
  (advice-add #'lsp :around #'lib-util/inhibit-message)
  (advice-add #'lsp-deferred :around #'lib-util/inhibit-message)

  (with-eval-after-load 'evil
    (evil-add-command-properties #'lsp-find-definition :jump t)
    (evil-add-command-properties #'lsp-find-implementation :jump t)
    (evil-add-command-properties #'lsp-find-declaration :jump t)
    (evil-add-command-properties #'lsp-find-type-definition :jump t)
    (evil-add-command-properties #'lsp-find-definition-mouse :jump t)))

(provide 'pkg-lsp-mode)
