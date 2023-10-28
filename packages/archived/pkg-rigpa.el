;;; -*- lexical-binding: t; -*-
;; Copyright (C) 2021 Icaro Motta

;; Author: Icaro Motta <icaro.ldm@gmail.com>
;; URL: http://github.com/ilmotta/dotfiles
;; Version: 1.0.0

;;; Commentary:
;;
;; All of the packages in this namespace are disabled because rigpa and symex
;; are under heavy developmen. The packages have a long way to go to be
;; flexible. For example, some commands fail if I don't have `line-number-mode'
;; enabled. I also don't like that it depends on packages I don't care about,
;; like centaur-tabs.
;;
;; I'm keeping the configuration because I believe rigpa can be quite
;; revolutionary to my workflows with its concept of towers (stacks) of modes.
;;

;;; Code:
(require 'use-package)

;; Non-specified dependency by rigpa.
(lib-util/pkg dynamic-ring
  :disabled t
  :straight t)

;; Non-specified dependency by rigpa.
(lib-util/pkg dynaring
  :disabled t
  :straight (:type git :host github :repo "countvajhula/dynaring"))

;; An evil way to edit Lisp symbolic expressions ("symexes") as trees.
(lib-util/pkg symex
  :disabled t
  :straight t
  :config
  ;; The default keybindings in symex mode treat increasingly nested code as
  ;; being "higher" and elements closer to the root as "lower." Think going "up"
  ;; to the nest and "down" to the root.
  (setq symex--user-evil-keyspec
        '(("j" . symex-go-up)
          ("k" . symex-go-down)
          ("C-j" . symex-climb-branch)
          ("C-k" . symex-descend-branch)
          ("M-j" . symex-goto-highest)
          ("M-k" . symex-goto-lowest)))

  (symex-initialize))

;; Modular editing levels and towers.
(lib-util/pkg rigpa
  :disabled t
  :straight (:type git :host github :repo "countvajhula/rigpa")
  :after (evil symex)
  :general
  (:prefix "SPC"
   :states '(normal)
   "<escape>" (lambda ()
                (interactive)
                (when (eq rigpa--complex rigpa-meta-complex)
                  (rigpa-exit-mode-mode))
                (rigpa-enter-tower-mode))))

(provide 'pkg-rigpa)
