;;; -*- lexical-binding: t; -*-

;;; Commentary:
;; https://github.com/minad/marginalia shows annotations based on different
;; completion categories.

;;; Code:

(require 'lib-util)

(defun pkg-marginalia/persist-annotator-registry ()
  (let ((inhibit-message t))
    (customize-save-variable 'marginalia-annotator-registry
                             marginalia-annotator-registry)))

(lib-util/pkg marginalia
  :elpaca (:host github
           :repo "minad/marginalia"
           :ref "a527fb03b76a2bce1e360c6e73a095e06922c3f3")
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
