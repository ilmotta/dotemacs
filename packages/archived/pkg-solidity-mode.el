;;; -*- lexical-binding: t; -*-
(require 'use-package)

;; Solidity is an object-oriented, high-level language for implementing smart
;; contracts.
(lib-util/pkg solidity-mode
  :straight t
  :defer t
  :config
  (setq solidity-comment-style 'slash))

(lib-util/pkg solidity-flycheck
  :straight t
  :defer t
  :hook (solidity-mode-hook . pkg-solidity-mode/setup-flycheck)
  :preface

  ;; This is a workaround because `solidity-flycheck' calls the `remove-if-not'
  ;; function, now called `cl-remove-if-not'.
  (defalias 'remove-if-not 'cl-remove-if-not)

  (defun pkg-solidity-mode/setup-flycheck ()
    ;; Both variables must be set before loading `solidity-flycheck' because it
    ;; configures flycheck at require time and unfortunately does not provide a
    ;; function to delay setup.
    (setq solidity-flycheck-solc-checker-active t)
    (setq solidity-flycheck-solium-checker-active t)
    (require 'solidity-flycheck)))

(lib-util/pkg company-solidity
  ;; Currently disabled because it always adds 'company-solidity to the
  ;; `company-backends' variable.
  :disabled t

  :straight t
  :defer t
  :after solidity-mode
  :config
  (with-eval-after-load 'company
    ;; Unfortunately installing company-solidity has a side-effect of mutating
    ;; `company-backends', so first we need to remove it from the list.
    (setq-default company-backends
                  (delq! 'company-solidity company-backends))
    (pkg-company/set-backend 'solidity-mode 'company-solidity)))

(provide 'pkg-solidity-mode)
