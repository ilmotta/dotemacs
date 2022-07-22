;;; -*- lexical-binding: t; -*-

(defun pkg-helpful/-navigate (original-fn button)
  "Force all push buttons to reuse same window."
  (let ((same-window-regexps ".*"))
    (funcall original-fn button)))

(my/package helpful
  :straight t
  :defer t

  :init
  (general-def
    :keymaps 'help-map
    "." #'helpful-at-point)

  (general-def
    [remap describe-function] #'helpful-callable
    [remap describe-variable] #'helpful-variable
    [remap describe-key] #'helpful-key
    [remap Info-goto-emacs-command-node] #'helpful-function
    [remap describe-coding-system] #'helpful-command)

  :config
  (advice-add #'helpful--navigate :around #'pkg-helpful/-navigate))

(provide 'pkg-helpful)
