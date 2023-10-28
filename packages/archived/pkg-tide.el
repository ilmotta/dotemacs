;;; -*- lexical-binding: t; -*-

(lib-util/pkg tide
  :straight t
  :defer t
  :hook ((typescript-mode-hook rjsx-mode-hook) . tide-setup)

  :init
  ;; Let me handle it.
  (setq tide-completion-setup-company-backend nil)

  (setq tide-completion-detailed nil
        tide-always-show-documentation nil)

  ;; Fix #1792: by default, tide ignores payloads larger than 100kb. This
  ;; is too small for larger projects that produce long completion lists,
  ;; so we up it to 512kb.
  (setq tide-server-max-response-length (* 512 1024))

  :config
  (with-eval-after-load 'evil
    (evil-add-command-properties #'tide-jump-to-definition :jump t)
    (evil-add-command-properties #'tide-jump-to-implementation :jump t)))

(provide 'pkg-tide)
