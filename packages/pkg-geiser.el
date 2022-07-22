;;; -*- lexical-binding: t; -*-

;;; Commentary:
;; A collection of major and minor modes for different Scheme interpreters. It
;; draws inspiration from environments such as Common Lisp's Slime, Factor's
;; FUEL,Squeak or Emacs itself.

;;; Code:

(defun pkg-geiser/outline-minor-mode-h ()
  (when (derived-mode-p 'scheme-mode)
    (setq-local outline-regexp my/outline-regex-lisp)))

(my/package geiser
  :no-require t
  :defer t

  :hook (scheme-mode-hook . outline-minor-mode)
  :hook (outline-minor-mode-hook . pkg-geiser/outline-minor-mode-h)

  :init
  (general-def
    :keymaps 'geiser-mode-map
    ;; This binding should be removed because it replaces C-c C-k used by
    ;; `org-edit-src-abort' in buffers with the `org-src-mode' enabled.
    "C-c C-k" nil)

  (setq geiser-default-implementation 'mit
        geiser-repl-history-filename (concat my/cache-dir "geiser_history"))

  :config
  ;; Augment Guile’s load path so that it finds source files from my fork.
  (with-eval-after-load 'guiser-guile
    (add-to-list 'geiser-guile-load-path (expand-file-name "~/data/repos/repos-forks/guix"))))

(provide 'pkg-geiser)
