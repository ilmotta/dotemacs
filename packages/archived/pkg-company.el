;;; -*- lexical-binding: t; -*-
(require 'use-package)

(defvar pkg-company/backend-alist
  '((text-mode (:separate company-dabbrev company-ispell))
    (prog-mode company-capf)
    (ledger-mode company-capf)
    (conf-mode company-capf company-dabbrev-code company-files))
  "An alist matching modes to company backends. The backends for any mode is
built from this.")

;;;###autoload
(defun pkg-company/set-backend (modes &rest backends)
  "Prepends BACKENDS (in order) to `company-backends' in MODES.

MODES should be one symbol or a list of them, representing major or minor modes.
This will overwrite backends for MODES on consecutive uses.

If the car of BACKENDS is nil, unset the backends for MODES.

Examples:

  (pkg-company/set-backend 'js-mode
    'company-tide 'company-yasnippet)

  (pkg-company/set-backend 'sh-mode
    '(company-shell :with company-yasnippet))

  (pkg-company/set-backend '(c-mode c++-mode)
    '(:separate company-irony-c-headers company-irony))

  (pkg-company/set-backend 'sh-mode nil)  ; unsets backends for sh-mode"
  (declare (indent defun))
  (dolist (mode (if (listp modes) modes (list modes)))
    (if (null (car backends))
        (setq pkg-company/backend-alist
              (delq (assq mode pkg-company/backend-alist)
                    pkg-company/backend-alist))
      (setf (alist-get mode pkg-company/backend-alist)
            backends))))

(lib-util/pkg company
  :straight t
  :disabled t
  :demand t

  :hook (evil-local-mode-hook . pkg-company/raise-keymaps-priorities)

  ;; Drastically reduce stuttering when scrolling over candidates.
  :hook (company-completion-started-hook . my/increase-gc-threshold)
  :hook (company-after-completion-hook . my/reset-gc-threshold)

  :hook (after-change-major-mode-hook . pkg-company/setup-text-mode-h)

  :general
  (:states 'insert "C-SPC" #'company-complete-common)

  :preface
  (defun pkg-company/setup-text-mode-h ()
    (when (derived-mode-p 'text-mode)
      ;; Disable auto-completion.
      (setq-local company-idle-delay nil)))

  (defun pkg-company/init-backends-h ()
    "Set `company-backends' for the current buffer."
    (or (memq major-mode '(fundamental-mode special-mode))
        buffer-read-only
        (lib-util/temp-buffer-p (or (buffer-base-buffer) (current-buffer)))
        (let (backends)
          (let ((mode major-mode)
                (modes (list major-mode)))
            (while (setq mode (get mode 'derived-mode-parent))
              (push mode modes))
            (dolist (mode modes)
              (dolist (backend (append (cdr (assq mode pkg-company/backend-alist))
                                       (default-value 'company-backends)))
                (push backend backends)))
            (setq-local company-backends
                        (delete-dups
                         (append (cl-loop for (mode . backends) in pkg-company/backend-alist
                                          if (or (eq major-mode mode) ; major modes
                                                 (and (boundp mode)
                                                      (symbol-value mode))) ; minor modes
                                          append backends)
                                 (nreverse backends))))))))

  (defun pkg-company/raise-keymaps-priorities ()
    "Fix #1335 in Doom Emacs.

NOTE Fix #1335: ensure `company-emulation-alist' is the first
item of `emulation-mode-map-alists', thus higher priority than
keymaps of evil-mode. We raise the priority of company-mode
keymaps unconditionally even when completion is not activated.
This should not cause problems, because when completion is
activated, the value of `company-emulation-alist' is ((t .
company-my-keymap)), when completion is not activated, the value
is ((t . nil)).
https://github.com/hlissner/doom-emacs/issues/1335

This function also fixes the CIDER issue #2908 where C-j and C-k
don't call the candidate selection functions.
https://github.com/clojure-emacs/cider/issues/2908"
    (when (memq 'company-emulation-alist emulation-mode-map-alists)
      (company-ensure-emulation-alist)))

  :init
  ;; Buffer-local backends will be computed when loading a major mode, so only
  ;; specify a global default here.
  (setq company-backends '(company-capf))

  ;; Only search the current buffer for `company-dabbrev' (a backend that
  ;; suggests text your open buffers). This prevents Company from causing lag
  ;; once you have a lot of buffers open. The default is `all'.
  (setq company-dabbrev-other-buffers nil)

  ;; Make `company-dabbrev' fully case-sensitive, to improve UX with
  ;; domain-specific words with particular casing.
  (setq company-dabbrev-downcase nil
        company-dabbrev-ignore-case nil)

  ;; Do not show quick-access hints beside the candidates, such as the candidate
  ;; index.
  (setq company-show-quick-access nil)

  ;; The default length of 3 is too big and 1 is too small, which would bring
  ;; too many unnecessary candidates.
  (setq company-minimum-prefix-length 2)

  ;; This can be overridden by the backend.
  (setq company-require-match 'never)

  ;; When `global-company-mode' is enabled all modes will use company-mode
  ;; except the ones listed after `not'.
  (setq company-global-modes
        (cons 'not
              '(erc-mode
                gud-mode
                help-mode
                message-mode
                vterm-mode)))

  (setq company-frontends
        '(company-pseudo-tooltip-frontend ; Always show candidates in overlay tooltip
          company-echo-metadata-frontend))

  ;; These auto-complete the current selection when `company-auto-commit-chars'
  ;; is typed. This is too magical. We already have the much more explicit RET
  ;; and TAB.
  (setq company-auto-commit nil)

  (setq company-tooltip-minimum-width 50
        company-tooltip-minimum 5
        company-tooltip-maximum-width 50
        company-tooltip-limit 10
        company-tooltip-align-annotations t
        company-tooltip-flip-when-above nil
        company-tooltip-offset 0
        company-tooltip-offset-display 'scrollbar)

  ;; Hack: disable company icons once and for all and fix line height issues.
  (setq company-format-margin-function nil)

  (setq company-idle-delay 0.1
        company-tooltip-idle-delay 0.1
        ;; Avoid setting to ~zero. For instance, when you're holding the
        ;; keybinding to scroll you don't need to display anything in the echo
        ;; area because you won't be able to read it.
        company-echo-delay 0.1)

  :config
  (defun my/company-capf-just-one-face (fn &rest args)
    (let ((orderless-match-faces [completions-common-part]))
      (apply fn args)))

  ;; The matching portions of candidates aren’t highlighted when orderless is
  ;; used. That’s because company-capf is hard-coded to look for the
  ;; completions-common-part face, and it only use one face, company-echo-common
  ;; to highlight candidates.
  (with-eval-after-load 'orderless
    (advice-add 'company-capf--candidates :around #'my/company-capf-just-one-face))

  (add-hook 'after-change-major-mode-hook #'pkg-company/init-backends-h 'append)
  (put 'pkg-company/init-backends-h 'permanent-local-hook t)

  (global-company-mode +1))

(provide 'pkg-company)
