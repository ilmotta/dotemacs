;;; -*- lexical-binding: t; -*-

;;; Commentary:
;; Highlights the current line.

;;; Code:

(defvar-local pkg-hl-line/mode-on nil
  "Store the hl-line-mode state for the current buffer.

Disable `hl-line-mode' when selecting text because the selection
is already visible.")

(defun pkg-hl-line/on-maybe ()
  (when pkg-hl-line/mode-on
    (hl-line-mode +1)))

(defun pkg-hl-line/off-maybe ()
  (when pkg-hl-line/mode-on
    (hl-line-mode -1)))

(defun pkg-hl-line/update-mode-state ()
  (when hl-line-mode
    (setq pkg-hl-line/mode-on t)))

(my/package hl-line
  :straight (:type built-in)
  :defer t

  :hook ((dired-mode-hook
          prog-mode-hook
          text-mode-hook
          conf-mode-hook
          org-agenda-mode-hook
          tablist-mode-hook) . hl-line-mode)
  :hook ((evil-visual-state-entry-hook
          activate-mark-hook) . pkg-hl-line/off-maybe)
  :hook ((evil-visual-state-exit-hook
          deactivate-mark-hook) . pkg-hl-line/on-maybe)
  :hook (hl-line-mode-hook . pkg-hl-line/update-mode-state)

  :init
  ;; Not having to render the hl-line overlay in multiple buffers offers a tiny
  ;; performance boost. I also don't need to see it in other buffers.
  (setq hl-line-sticky-flag nil
        global-hl-line-sticky-flag nil)

  ;; Emacs28
  (setq hl-line-overlay-priority -50))

(provide 'pkg-hl-line)
