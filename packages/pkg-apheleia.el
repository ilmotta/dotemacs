;;; -*- lexical-binding: t; -*-

(cl-defun pkg-apheleia/-formatter-async
    (&key cmd-builder buffer scratch formatter callback remote async &allow-other-keys)
  (let* ((inhibit-message t)
         ;; Make the command run in the buffer's `default-directory'.
         (default-directory (with-current-buffer buffer default-directory)))
    (when async
      (with-current-buffer scratch
        (erase-buffer))
      (let ((proc (make-process
                   :name "apheleia-format-buffer"
                   :buffer scratch
                   :command (funcall cmd-builder)
                   :connection-type 'pipe
                   :sentinel (lambda (proc state)
                               (if (zerop (process-exit-status proc))
                                   (funcall callback)
                                 (message "Could not format buffer. Error '%s'\n%s"
                                          state
                                          (with-current-buffer scratch
                                            (buffer-string))))))))
        (with-current-buffer buffer
          (process-send-region proc (point-min) (point-max))
          (process-send-eof proc))))))

(cl-defun pkg-apheleia/formatter-ledger
    (&key buffer scratch formatter callback remote async &allow-other-keys)
  (with-current-buffer scratch
    (ledger-mode-clean-buffer))
  (funcall callback))

(cl-defun pkg-apheleia/formatter-nixfmt
    (&key buffer scratch formatter callback remote async &allow-other-keys)
  (pkg-apheleia/-formatter-async
   :cmd-builder (lambda ()
                  (list (executable-find "nixfmt") "--width" "80"))
   :buffer buffer
   :scratch scratch
   :formatter formatter
   :callback callback
   :remote remote
   :async async))

;; Apheleia is a code formatting tool that can reliably and efficiently run on
;; every save.
(my/package apheleia
  :straight (:type git :host github :repo "raxod502/apheleia")
  :defer t

  :hook (nix-mode-hook . apheleia-mode)

  :init
  (general-def
    :keymaps 'pkg-emacs/file-command-map
    "=" #'apheleia-format-buffer)

  ;; I don't care too much about the precision of the cursor position. The
  ;; algorithm has quadratic performance (terrible), so I prefer to set it to a
  ;; lower value. The best I can do is use it to format the buffer on demand
  ;; with a keybinding.
  (setq apheleia-max-alignment-size 100)

  ;; Uncomment to debug commands.
  (setq apheleia-log-only-errors nil)

  (setq apheleia-mode-alist
        '((clojure-mode . zprint)
          (clojurescript-mode . zprint)
          (css-mode . prettier)
          (elixir-mode . mix-format)
          (go-mode . gofmt)
          (html-mode . prettier)
          (java-mode . google-java-format)
          (js-mode . prettier)
          (json-mode . prettier)
          (ledger-mode . ledger)
          (nix-mode . nixfmt)
          (python-mode . black)
          (ruby-mode . prettier)
          (rust-mode . rustfmt)
          (rustic-mode . rustfmt)
          (sass-mode . prettier)
          (terraform-mode . terraform)
          (typescript-mode . prettier)
          (typescript-tsx-mode . prettier)
          (web-mode . prettier)
          (yaml-mode . prettier)))

  (setq apheleia-formatters
        `((black "black" "-")
          (gofmt "gofmt")
          (google-java-format "google-java-format" "-")
          (isort "isort" "--stdout" "-")
          (latexindent "latexindent")
          (ledger . pkg-apheleia/formatter-ledger)
          (mix-format "mix" "format" "-")
          (nixfmt . pkg-apheleia/formatter-nixfmt)
          (ocamlformat "ocamlformat" "-" "--name" filepath)
          (rustfmt "rustfmt" "--unstable-features" "--skip-children" "--quiet" "--emit" "stdout")
          (terraform "terraform" "fmt" "-")
          (zprint "zprint" "{:search-config? true}")

          ;; Unfortunately, apheleia mutates the `apheleia-formatters' variable
          ;; to resolve the path to the binary when using "npx". This is a
          ;; problem, because if you run the formatter in project A (which has
          ;; prettier in package.json), and then runs the formatter in project B
          ;; (which happens to NOT have prettier in package.json), then
          ;; formatting on project B will use project A's binary. If project A
          ;; failed to install prettier or installed a faulty version, then
          ;; project A won't be able to format anything, even if there was a
          ;; global prettier installation.
          ;;
          ;; For the moment, I'm relying on a global prettier binary. Also this
          ;; variable can change per project using dir local variables.
          (prettier "prettier" "--stdin-filepath" filepath))))

(provide 'pkg-apheleia)
