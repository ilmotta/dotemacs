;;; -*- lexical-binding: t; -*-

;;; Commentary:
;; A fast and light package to highlight the text under cursor. While there are
;; many similar packages (see Other Packages) that have the same basic
;; functionality these they tend towards being heavier by supporting advanced
;; functionality. For example running operation on every key-stroke or marking
;; symbols over the entire buffer which can cause poor performance editing large
;; files.

;;; Code:

(require 'lib-util)

(defun pkg-idle-highlight-mode/after-change-major-mode-h ()
  (cond ((derived-mode-p 'emacs-lisp-mode)
         (setq-local idle-highlight-exceptions
                     '("defconst"
                       "defcustom"
                       "defmacro"
                       "defun"
                       "defvar"
                       "let"
                       "setq"
                       "setq-local")))
        ((derived-mode-p 'clojure-mode)
         (setq-local idle-highlight-exceptions
                     '("def"
                       "defmacro"
                       "defn"
                       "defonce"
                       "fn"
                       "let"
                       "ns")))))

(defun pkg-idle-highlight-mode/disable ()
  (when (derived-mode-p 'eshell-mode)
    (idle-highlight-mode -1)))

(lib-util/pkg idle-highlight-mode
  :elpaca (:host codeberg
           :repo "ideasman42/emacs-idle-highlight-mode"
           :ref "f9091c907d41e7b12d99d108a194229b8dbfc5ae")
  :defer t

  :hook (after-change-major-mode-hook . pkg-idle-highlight-mode/after-change-major-mode-h)
  :hook (eshell-mode-hook . pkg-idle-highlight-mode/disable)

  :init
  (setq idle-highlight-visible-buffers nil)
  (setq idle-highlight-idle-time 0.25)
  (setq idle-highlight-exclude-point t)

  ;; Ignore certain modes when `global-idle-highlight-mode' is enabled.
  (setq idle-highlight-ignore-modes '(csv-mode
                                      json-mode
                                      json-ts-mode
                                      org-mode))

  ;; Ignore read-only buffers
  (setq global-idle-highlight-ignore-buffer
        (lambda (buf)
          (with-current-buffer buf
            buffer-read-only)))

  :config
  (global-idle-highlight-mode +1))

(provide 'pkg-idle-highlight-mode)
