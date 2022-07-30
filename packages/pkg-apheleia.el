;;; -*- lexical-binding: t; -*-

(defun pkg-apheleia/format-ledger ()
  "Format and save current ledger-mode buffer.

Useful to be used as a buffer hook, such as `before-save-hook'.
Example:

  (add-hook 'before-save-hook #'pkg-apheleia/format-ledger nil t)
"
  (interactive)
  (apheleia-format-buffer 'ledger #'basic-save-buffer))

(cl-defun pkg-apheleia/-formatter-async-outdated
    (&key cmd-builder buffer scratch formatter callback remote async &allow-other-keys)
  "Kept for reference, it might be useful for commands that don't
understand standard input."
  (let* ((inhibit-message t)
         (tmp-file (make-temp-file (format "%s-scratch-" formatter)
                                   nil nil
                                   (with-current-buffer scratch
                                     (buffer-string))))
         ;; Make the command run in the buffer's `default-directory'.
         (default-directory (buffer-local-value 'default-directory buffer)))
    (when async
      (promise-then (lib-system/promise-start-process-shell-command (funcall cmd-builder tmp-file))
                    (lambda (formatted)
                      (with-current-buffer scratch
                        (erase-buffer)
                        (insert formatted))
                      (delete-file tmp-file)
                      (funcall callback))))))

(cl-defun pkg-apheleia/-formatter-async
    (&key cmd-builder buffer scratch formatter callback remote async &allow-other-keys)
  (let* ((inhibit-message t)
         ;; Make the command run in the buffer's `default-directory'.
         (default-directory (buffer-local-value 'default-directory buffer)))
    (when async
      (with-current-buffer scratch
        (erase-buffer))
      (let ((proc (make-process
                   :name "apheleia-format-buffer"
                   :buffer scratch
                   :command (funcall cmd-builder)
                   :connection-type 'pipe
                   :sentinel (lambda (_proc _state)
                               (funcall callback)))))
        (with-current-buffer buffer
          (process-send-region proc (point-min) (point-max))
          (process-send-eof proc))))))

(cl-defun pkg-apheleia/formatter-ledger
    (&key buffer scratch formatter callback remote async &allow-other-keys)
  (with-current-buffer scratch
    (ledger-mode-clean-buffer))
  (funcall callback))

(cl-defun pkg-apheleia/formatter-zprint
    (&key buffer scratch formatter callback remote async &allow-other-keys)
  (pkg-apheleia/-formatter-async
   :cmd-builder (lambda ()
                  (list (executable-find "zprint")))
   :buffer buffer
   :scratch scratch
   :formatter formatter
   :callback callback
   :remote remote
   :async async))

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
        '((c++-mode . clang-format)
          (c-mode . clang-format)
          (cc-mode . clang-format)
          (clojure-mode . zprint)
          (clojurescript-mode . zprint)
          (css-mode . prettier)
          (elixir-mode . mix-format)
          (go-mode . gofmt)
          (haskell-mode . brittany)
          (html-mode . prettier)
          (java-mode . google-java-format)
          (js-mode . prettier)
          (js3-mode . prettier)
          (json-mode . prettier)
          (LaTeX-mode . latexindent)
          (latex-mode . latexindent)
          (ledger-mode . ledger)
          (nix-mode . nixfmt)
          (python-mode . black)
          (ruby-mode . prettier)
          (rust-mode . rustfmt)
          (rustic-mode . rustfmt)
          (sass-mode . prettier)
          (terraform-mode . terraform)
          (TeX-latex-mode . latexindent)
          (TeX-mode . latexindent)
          (tuareg-mode . ocamlformat)
          (typescript-mode . prettier)
          (typescript-tsx-mode . prettier)
          (web-mode . prettier)
          (yaml-mode . prettier)))

  (setq apheleia-formatters
        `((black "black" "-")
          (brittany "brittany")
          (clang-format "clang-format")
          (gofmt "gofmt")
          (google-java-format "google-java-format" "-")
          (isort "isort" "--stdout" "-")
          (latexindent "latexindent")
          (ledger . pkg-apheleia/formatter-ledger)
          (mix-format "mix" "format" "-")
          (nixfmt . pkg-apheleia/formatter-nixfmt)
          (ocamlformat "ocamlformat" "-" "--name" filepath)
          (prettier npx "prettier" "--stdin-filepath" filepath)
          (rustfmt "rustfmt" "--unstable-features" "--skip-children" "--quiet" "--emit" "stdout")
          (terraform "terraform" "fmt" "-")
          (zprint . pkg-apheleia/formatter-zprint))))

(provide 'pkg-apheleia)
