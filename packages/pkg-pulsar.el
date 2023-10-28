;;; -*- lexical-binding: t; -*-

(require 'lib-util)

(lib-util/pkg pulsar
  :elpaca (:ref "57010e2c6cdee14acfd87b4c2bd75c796f04a75e")
  :defer t

  ;; This is mandatory when pulsar is deferred and `consult-after-jump-hook' is
  ;; set to `pulsar-reveal-entry'.
  :commands (pulsar-reveal-entry)

  :init
  (general-def
    :keymaps 'my/keys-mode-map
    "C-x l" #'pulsar-pulse-line
    "C-x L" #'pulsar-highlight-line)

  (setq pulsar-pulse t)
  (setq pulsar-delay 0.05)
  (setq pulsar-iterations 8)
  (setq pulsar-face 'pulsar-cyan)
  (setq pulsar-highlight-face 'pulsar-yellow)

  (setq pulsar-pulse-functions
        '(backward-page
          bookmark-jump
          delete-other-windows
          delete-window
          forward-page
          move-to-window-line-top-bottom
          org-backward-heading-same-level
          org-forward-heading-same-level
          org-next-visible-heading
          org-previous-visible-heading
          other-window
          outline-backward-same-level
          outline-forward-same-level
          outline-next-visible-heading
          outline-previous-visible-heading
          outline-up-heading
          recenter-top-bottom
          reposition-window))

  (with-eval-after-load 'consult
    (add-hook 'consult-after-jump-hook #'recenter)
    (add-hook 'consult-after-jump-hook #'pulsar-reveal-entry))

  (when (bound-and-true-p evil-mode)
    (setq pulsar-pulse-functions
          (append pulsar-pulse-functions '(evil-window-down
                                           evil-window-left
                                           evil-window-right
                                           evil-window-up)))))

(provide 'pkg-pulsar)
