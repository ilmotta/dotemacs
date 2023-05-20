;;; -*- lexical-binding: t; -*-

;;; Commentary:
;; Highlight surrounding parentheses.

;;; Code:

(my/package highlight-parentheses
  :elpaca (:ref "438a1cb2563e2a2496be4678cc0df8d5b22caf5d")
  :defer t
  :hook (prog-mode-hook . highlight-parentheses-mode)
  :init
  (setq hl-paren-delay 0.08)
  ;; Only the first parenthesis should be bold.
  (setq hl-paren-attributes '((:weight bold))))

(provide 'pkg-highlight-parentheses)
