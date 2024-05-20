;;; -*- lexical-binding: t; -*-

(require 'lib-util)

(defun pkg-doom-modeline/define-sections ()
  "Define mode line sections without the `modals' section.

Unfortunately `doom-modeline-def-modeline' is called
automatically with hardcoded sections and no variable allows me
to customize the section. There's probably a better way, but this
works fine."
  (doom-modeline-def-modeline 'main
    '(eldoc bar workspace-name
      ;; window-number
      ;; modals
      matches follow buffer-info remote-host buffer-position word-count parrot selection-info)
    '(compilation objed-state misc-info
      ;; persp-name
      battery grip
      ;; irc mu4e gnus
      github debug repl lsp
      ;; minor-modes
      input-method indent-info
      ;; buffer-encoding major-mode
      process
      ;; We need to remove vcs because doom-modeline throws error about the vcs section.
      ;; vcs
      ;; check
      time)))

(defun pkg-doom-modeline/setup-mode ()
  (doom-modeline-mode +1)
  ;; This setting doesn't take effect if present in the :init section.
  (setq doom-modeline-bar-width 0)
  (remove-hook 'find-file-hook #'doom-modeline-update-vcs))

(defun pkg-doom-modeline/-font-height-patch (&rest _args)
  "PATCH: Always return the specified `doom-modeline-height'."
  doom-modeline-height)

(defun pkg-doom-modeline/buffer-file-name-filter-return (result)
  "Transform buffer file name after doom-modeline.
Use this function to hardcode rules that can't be easily generalized."
  (let ((rx-status-mobile-context (rx line-start "status-mobile/src/status_im/contexts/")))
    (cond ((string-match-p rx-status-mobile-context result)
           (replace-regexp-in-string rx-status-mobile-context "mobile/ctx/" result))
          (t result))))

(lib-util/pkg doom-modeline
  :elpaca (:ref "65d0bd83eb7c393092e032c24b882f3ba19b4899")
  :defer t
  :hook (elpaca-after-init-hook . pkg-doom-modeline/setup-mode)

  :init
  ;; Determines the style used by `doom-modeline-buffer-file-name'.
  ;;
  ;; Given ~/Projects/FOSS/emacs/lisp/comint.el
  ;;   truncate-upto-project => ~/P/F/emacs/lisp/comint.el
  ;;   truncate-from-project => ~/Projects/FOSS/emacs/l/comint.el
  ;;   truncate-with-project => emacs/l/comint.el
  ;;   truncate-except-project => ~/P/F/emacs/l/comint.el
  ;;   truncate-upto-root => ~/P/F/e/lisp/comint.el
  ;;   truncate-all => ~/P/F/e/l/comint.el
  ;;   relative-from-project => emacs/lisp/comint.el
  ;;   relative-to-project => lisp/comint.el
  ;;   file-name => comint.el
  ;;   buffer-name => comint.el<2> (uniquify buffer name)
  ;;
  (setq doom-modeline-buffer-file-name-style 'relative-from-project)

  (setq doom-modeline-buffer-encoding nil
        doom-modeline-buffer-modification-icon t
        doom-modeline-check-icon nil
        doom-modeline-check-simple-format t
        doom-modeline-icon (display-graphic-p)
        doom-modeline-irc nil
        doom-modeline-lsp-icon nil
        doom-modeline-major-mode-icon t
        doom-modeline-minor-modes nil
        doom-modeline-modal nil
        doom-modeline-modal-icon nil
        doom-modeline-unicode-fallback nil
        doom-modeline-vcs-icon nil)

  ;; Don't display version in the modeline.
  (setq doom-modeline-env-enable-elixir nil
        doom-modeline-env-enable-go nil
        doom-modeline-env-enable-perl nil
        doom-modeline-env-enable-python nil
        doom-modeline-env-enable-ruby nil
        doom-modeline-env-enable-rust nil)

  :config
  (pkg-doom-modeline/define-sections)

  (advice-add #'doom-modeline--buffer-file-name :filter-return #'pkg-doom-modeline/buffer-file-name-filter-return)

  ;; Set the modeline height to the minimum possible value.
  (advice-add #'doom-modeline--font-height :override #'pkg-doom-modeline/-font-height-patch))

(provide 'pkg-doom-modeline)
