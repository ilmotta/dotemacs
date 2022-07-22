;;; -*- lexical-binding: t; -*-

;;; Commentary:
;; https://github.com/minad/marginalia shows annotations based on different
;; completion categories.

;;; Code:

(my/package marginalia
  :straight (:host github :repo "minad/marginalia" :branch "main")
  :defer t
  :hook (minibuffer-setup-hook . marginalia-mode)
  :init
  (general-def
    :keymaps 'minibuffer-local-map
    ;; Similar to the `dired-hide-details-mode' binding.
    "M-(" #'marginalia-cycle))

(provide 'pkg-marginalia)
