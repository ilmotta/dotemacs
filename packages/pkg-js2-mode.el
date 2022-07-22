;;; -*- lexical-binding: t; -*-

(my/package js2-mode
  :straight t
  :defer t

  :hook (js-mode-hook . js2-minor-mode)

  :init
  (general-def
    :keymaps 'js-mode-map
    :states '(normal insert emacs)
    "M-." #'xref-find-definitions)

  (setq js-indent-level 2
        js2-bounce-indent-p nil)

  ;; Disable all parse errors and warnings by default,
  ;; leaving room for flycheck to handle them.
  (setq js2-mode-show-parse-errors nil
        js2-mode-show-strict-warnings nil)

  ;; Disable warnings about missing semi-colons.
  ;; js2-missing-semi-one-line-override t
  ;; (setq js2-strict-missing-semi-warning nil)

  ;; Leaves syntax highlighting to tree-sitter.
  (setq js2-highlight-level 0)

  (setq js2-global-externs
        '("afterAll"
          "afterEach"
          "beforeAll"
          "beforeEach"
          "context"
          "describe"
          "it")))

(provide 'pkg-js2-mode)
