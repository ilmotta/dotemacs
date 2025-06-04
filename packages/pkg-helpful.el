;;; -*- lexical-binding: t; -*-

(require 'lib-util)

(defun pkg-helpful/-navigate (original-fn button)
  "Force all push buttons to reuse same window."
  (let ((same-window-regexps ".*"))
    (funcall original-fn button)))

(lib-util/pkg helpful
  :ensure (:ref "94c25337b2de2f9da60914a7c0c6cca9584c0231")
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
