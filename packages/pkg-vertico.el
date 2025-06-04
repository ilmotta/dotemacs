;;; -*- lexical-binding: t; -*-

;;; Code:

(require 'lib-util)

(lib-util/pkg vertico
  :ensure (:host github
           :repo "minad/vertico"
           :ref "ac82acf177a0dfc97deac8626a8a98c06bffc96c"
           :files (:defaults "extensions/*.el"))
  :defer t
  :hook (elpaca-after-init-hook . vertico-mode)

  :init
  (general-def
    :keymaps 'vertico-map
    my/consult-preview-and-previous-line-key #'previous-line
    my/consult-preview-and-next-line-key #'next-line
    "M-f" #'vertico-multiform-flat
    "M-g" #'vertico-multiform-grid
    "M-r" #'vertico-multiform-reverse
    "M-u" #'vertico-multiform-unobtrusive
    "M-v" #'vertico-multiform-vertical)

  (with-eval-after-load 'evil
    (general-def
      :keymaps 'vertico-map
      "C-j" #'next-line
      "C-k" #'previous-line)

    (general-def
      :keymaps 'vertico-map
      :states '(insert emacs)
      "<escape>" #'abort-minibuffers
      "C-j"      #'next-line
      "C-k"      #'previous-line))

  (setq vertico-cycle nil)
  (setq vertico-resize 'grow-only)
  (setq vertico-count 10)
  (setq vertico-scroll-margin 0)
  (setq vertico-count-format nil)

  (setq vertico-multiform-categories
        '(;; Operates on commands such as `consult-grep', `consult-git-grep' and
          ;; `consult-ripgrep'.
          ;; (consult-grep buffer)
          ))

  :config
  (require 'vertico-buffer)
  (require 'vertico-flat)
  (require 'vertico-grid)
  (require 'vertico-multiform)
  (require 'vertico-reverse)
  (require 'vertico-unobtrusive)

  (vertico-multiform-mode +1))

(provide 'pkg-vertico)
