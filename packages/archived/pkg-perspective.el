;;; -*- lexical-binding: t; -*-

;;; Commentary:
;; I tried to use this package on 2022-08-11, but it didn't bring much value for
;; me because a big part of its functionality is already covered by
;; `tab-bar-mode' and the `doom-modeline' package (it shows tab names or
;; sequential numbers). It has a nice integration with `consult', but I felt it
;; doesn't solve my main problem, which is to quickly manage multiple shell
;; buffers.

;;; Code:
(my/package perspective
  :straight t
  :defer t

  :hook (after-init-hook . persp-mode)

  :init
  (setq persp-suppress-no-prefix-key-warning t)
  (setq persp-state-default-file (concat my/cache-dir "perspective"))

  ;; Latest first.
  (setq persp-sort 'created)

  (with-eval-after-load 'consult
    (consult-customize consult--source-buffer :hidden t :default nil)
    (add-to-list 'consult-buffer-sources 'persp-consult-source))

  (setq persp-switch-wrap t)
  (setq persp-modestring-short t)

  :config
  (general-def
    :keymaps 'my/keys-mode-map
    "s-c" '(:keymap perspective-map :package perspective)))

(provide 'pkg-perspective)
