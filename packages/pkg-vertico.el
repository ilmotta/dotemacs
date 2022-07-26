;;; -*- lexical-binding: t; -*-
;;; Code:

(eval-and-compile
  (defun pkg-vertico/load-paths ()
    (list (concat (straight--repos-dir "vertico") (file-name-as-directory "extensions")))))

(my/package vertico
  :straight (:host github :repo "minad/vertico")
  :load-path (lambda () (pkg-vertico/load-paths))
  :defer t

  :hook (after-init-hook . vertico-mode)

  :init
  (general-def
    :keymaps 'vertico-map
    "M-k" #'previous-line
    "M-j" #'next-line
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

  (setq vertico-multiform-commands
        '((execute-extended-command flat)
          (lib-media/google-tts-read-region flat)
          (consult-mode-command flat)))

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
