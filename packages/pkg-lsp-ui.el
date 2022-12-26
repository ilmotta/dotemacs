;;; -*- lexical-binding: t; -*-

(my/package
  (lsp-ui :ref "fb1073013f745bce056811a38e2b0b8b2a4b5ebc")
  :defer t
  :init
  (setq lsp-ui-peek-enable nil)

  ;; Documentation
  (setq lsp-ui-doc-enable t
        lsp-ui-doc-alignment 'frame
        lsp-ui-doc-delay 0.1
        lsp-ui-doc-enhanced-markdown t
        lsp-ui-doc-header nil
        lsp-ui-doc-include-signature t
        lsp-ui-doc-max-height 20
        lsp-ui-doc-max-width 70
        lsp-ui-doc-position 'at-point
        lsp-ui-doc-render-function nil ; Render as markdown
        lsp-ui-doc-show-with-cursor nil
        lsp-ui-doc-show-with-mouse t
        lsp-ui-doc-text-scale-level 0
        lsp-ui-doc-use-webkit nil
        lsp-ui-doc-winum-ignore t)

  ;; Sideline
  (setq lsp-ui-sideline-enable t
        lsp-ui-sideline-actions-icon nil
        lsp-ui-sideline-delay 0.5
        lsp-ui-sideline-diagnostic-max-line-length 70
        lsp-ui-sideline-diagnostic-max-lines 1
        lsp-ui-sideline-ignore-duplicate t
        lsp-ui-sideline-show-code-actions nil
        lsp-ui-sideline-show-diagnostics t
        ;; Don't show symbol definitions in the sideline. They are pretty noisy, and
        ;; there is a bug preventing Flycheck errors from being shown (the errors
        ;; flash briefly and then disappear).
        lsp-ui-sideline-show-hover nil
        lsp-ui-sideline-show-symbol t
        lsp-ui-sideline-update-mode 'line
        lsp-ui-sideline-wait-for-all-symbols t))

(provide 'pkg-lsp-ui)
