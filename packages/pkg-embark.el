;;; -*- lexical-binding: t; -*-

;;; Commentary:
;; https://github.com/oantolin/embark provides a sort of right-click contextual
;; menu for Emacs, accessed through the `embark-act' command, offering you
;; relevant actions to use on a target determined by its context. This package
;; is necessary to have create occur buffers with Selectrum candidates.

;;; Code:

(my/package embark
  :straight t
  :defer t

  :init
  (general-def
    :keymaps 'help-map
    "b" #'embark-bindings)

  (general-def
    ;; If you want a reminder of which actions are available after running
    ;; embark-act type C-h which will prompt you for an action with completion,
    ;; and remind you of the keybindings.
    "C-." #'embark-act
    "C-," #'embark-dwim)

  ;; Replace help command with a completing-read interface.
  (setq prefix-help-command #'embark-prefix-help-command)

  ;; Explicitly set key to avoid warning "No cycling possible; press C-, again
  ;; to act."
  (setq embark-cycle-key (kbd "C-h"))

  (setq embark-indicators '(embark-minimal-indicator
                            embark-highlight-indicator
                            embark-isearch-highlight-indicator))

  ;; Use the keymap prompter. To display the completing read interface, press C-h.
  (setq embark-prompter 'embark-keymap-prompter)

  :config
  (with-eval-after-load 'helpful
    (general-def
      :keymaps 'embark-symbol-map
      "h" #'helpful-at-point)))

;; If you use the grepping commands from the Consult package, consult-grep,
;; consult-git-grep or consult-ripgrep, then you’ll probably want to install and
;; load the embark-consult package, which adds support for exporting a list of
;; grep results to an honest grep-mode buffer, on which you can even use wgrep
;; if you wish.
(my/package embark-consult
  :straight t
  :defer t
  :init
  (with-eval-after-load 'embark
    (with-eval-after-load 'consult
      (require 'embark-consult))))

(defun pkg-embark/setup-org ()
  (require 'embark-org))

(my/package embark-org
  :straight (:type git :flavor melpa :files ("embark-org.el") :host github :repo "oantolin/embark")
  :defer t
  :hook (org-load-hook . pkg-embark/setup-org))

(provide 'pkg-embark)
