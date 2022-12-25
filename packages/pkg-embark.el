;;; -*- lexical-binding: t; -*-

;;; Commentary:
;; https://github.com/oantolin/embark provides a sort of right-click contextual
;; menu for Emacs, accessed through the `embark-act' command, offering you
;; relevant actions to use on a target determined by its context. This package
;; is necessary to have create occur buffers with Selectrum candidates.

;;; Code:

(my/package embark
  ;; We need to explicitly add embark-consult.el.
  :straight (:host github :repo "oantolin/embark" :files ("embark.el" "embark-org.el" "embark-consult.el"))
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

(provide 'pkg-embark)
