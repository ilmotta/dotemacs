;;; -*- lexical-binding: t; -*-

;; Highlight surrounding parentheses.
(my/package highlight-parentheses
  :straight t
  :defer t
  :hook (prog-mode-hook . highlight-parentheses-mode)
  :init
  (setq hl-paren-delay 0.08)
  ;; Only the first parenthesis should be bold.
  (setq hl-paren-attributes '((:weight bold))))

(provide 'pkg-highlight-parentheses)
