;;; -*- lexical-binding: t; -*-

;;; Commentary:
;; https://github.com/minad/marginalia shows annotations based on different
;; completion categories.

;;; Code:

(defun pkg-marginalia/persist-annotator-registry ()
  (let ((inhibit-message t))
    (customize-save-variable 'marginalia-annotator-registry
                             marginalia-annotator-registry)))

(my/package marginalia
  :straight (:host github :repo "minad/marginalia" :branch "main")
  :defer t

  :hook (minibuffer-setup-hook . marginalia-mode)

  :init
  (general-def
    :keymaps 'minibuffer-local-map
    ;; Similar to the `dired-hide-details-mode' binding.
    "M-(" #'marginalia-cycle)

  :config
  (advice-add #'marginalia-cycle :after #'pkg-marginalia/persist-annotator-registry))

(provide 'pkg-marginalia)
