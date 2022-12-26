;;; -*- lexical-binding: t; -*-

;;; Code:

(defun pkg-corfu/disable-auto-complete ()
  "Disable auto-complete for the current buffer."
  (setq-local corfu-auto nil))

(defun pkg-corfu/quit ()
  (interactive)
  (corfu-quit)
  (evil-normal-state))

(defun pkg-corfu/complete-or-insert ()
  (interactive)
  (cond ((equal major-mode 'eshell-mode)
         ;; On `eshell-mode' we don't want to use `corfu-insert' because it
         ;; inserts an extra whitespace after completing diretories (just one
         ;; example).
         (call-interactively #'corfu-complete))
        (:default
         (call-interactively #'corfu-insert))))

(defun pkg-corfu/complete-in-minibuffer ()
  (interactive)
  (if (fboundp #'consult-completion-in-region)
      (let ((completion-extra-properties corfu--extra)
            completion-cycle-threshold completion-cycling)
        (apply #'consult-completion-in-region completion-in-region--data))
    (message "Package consult is not loaded")))

;; Corfu is the minimalistic completion-in-region counterpart of the Vertico
;; minibuffer UI. Corfu is a small package, which relies on the Emacs completion
;; facilities and concentrates on providing a polished completion UI.
(my/package
  (corfu :ref "7bf3ec4622372ed23e83a0778ded53222c4e1187")
  :defer t
  :hook (elpaca-after-init-hook . global-corfu-mode)

  :init
  (general-def
    :keymaps 'corfu-map
    "M-m"      #'pkg-corfu/complete-in-minibuffer
    "<tab>"    #'pkg-corfu/complete-or-insert
    "<escape>" #'pkg-corfu/quit
    "<return>" #'pkg-corfu/return-handler)

  ;; See https://github.com/minad/corfu/issues/12 for more details on why the
  ;; keybindings are defined this way.
  (general-def
    :keymaps 'completion-in-region-mode
    :definer 'minor-mode
    :states '(insert emacs)
    :predicate 'corfu-mode
    "C-j"      #'corfu-next
    "C-k"      #'corfu-previous
    "<tab>"    #'pkg-corfu/complete-or-insert
    "<escape>" #'pkg-corfu/quit
    "<return>" #'pkg-corfu/return-handler)

  (setq corfu-excluded-modes '(eshell-mode))

  (setq corfu-auto nil)
  (setq corfu-auto-delay 0.06)
  (setq corfu-auto-prefix 2)
  (setq corfu-bar-width 0.50)
  (setq corfu-count 12)
  (setq corfu-cycle nil)
  (setq corfu-echo-documentation 0.25)
  (setq corfu-left-margin-width 0.5)
  (setq corfu-max-width 60)
  (setq corfu-min-width 50)
  (setq corfu-preselect-first t)
  (setq corfu-preview-current nil)
  (setq corfu-quit-at-boundary 'separator)
  (setq corfu-quit-no-match 'separator)
  (setq corfu-right-margin-width 0.5))

(provide 'pkg-corfu)
