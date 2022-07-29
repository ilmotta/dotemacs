;;; -*- lexical-binding: t; -*-

;;; Code:
;; Archived on 2022-07-29 because certain commands don't behave as I want, and
;; customizing the package proved to be much more difficult.
;;
;; For example, `detached-open-session' sometimes selects the output buffer,
;; other times it doesn't.
;;
;; The consult integration is great, but it still lacks the preview
;; functionality, which would be very useful, as often times I just want to see
;; the latest output without messing with the recently opened buffers.
;;
;; There are other inconsistencies that make using detach a pain, for instance,
;; to terminate a session, sometimes I first need to kill it and only then
;; delete the it.

(my/package detached
  :straight t
  :defer t
  :init
  (general-def
    :keymaps 'my/keys-mode-map
    :states '(normal insert emacs visual)
    :prefix my/leader
    :non-normal-prefix my/non-normal-prefix
    "d" '(:keymap detached-action-map :package detached))

  (general-def
    [remap async-shell-command] #'detached-shell-command
    [remap compile] #'detached-compile
    [remap detached-open-session] #'detached-consult-session)

  (setq detached-db-directory (concat my/cache-dir "detached/"))
  (setq detached-session-directory "/tmp/detached")

  ;; Number of context lines to display for a session.
  (setq detached-session-context-lines 50)

  :config
  (detached-init)

  (with-eval-after-load 'consult
    (require 'detached-consult)
    (consult--define-state detached))

  (with-eval-after-load 'embark
    (general-def
      :keymaps 'embark-detached-map
      "o" #'detached-open-session)))

(provide 'pkg-detached)
