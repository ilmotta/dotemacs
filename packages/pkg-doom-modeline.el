;;; -*- lexical-binding: t; -*-

(defun pkg-doom-modeline/define-sections ()
  "Define mode line sections without the `modals' section.

Unfortunately `doom-modeline-def-modeline' is called
automatically with hardcoded sections and no variable allows me
to customize the section. There's probably a better way, but this
works fine."
  (doom-modeline-def-modeline 'main
    '(bar workspace-name window-number matches follow buffer-info remote-host buffer-position word-count parrot selection-info)
    '(objed-state misc-info persp-name battery grip irc mu4e gnus github debug repl lsp minor-modes input-method indent-info buffer-encoding major-mode process
      ;; vcs
      checker time))

  (doom-modeline-def-modeline 'minimal
    '(bar matches buffer-info-simple)
    '(media-info major-mode))

  (doom-modeline-def-modeline 'special
    '(bar window-number matches buffer-info buffer-position word-count parrot selection-info)
    '(objed-state misc-info battery irc-buffers debug minor-modes input-method indent-info buffer-encoding major-mode process))

  (doom-modeline-def-modeline 'project
    '(bar workspace-name window-number buffer-default-directory)
    '(misc-info battery irc mu4e gnus github debug minor-modes input-method major-mode process))

  (doom-modeline-def-modeline 'dashboard
    '(bar window-number buffer-default-directory-simple)
    '(misc-info battery irc mu4e gnus github debug minor-modes input-method major-mode process))

  (doom-modeline-def-modeline 'vcs
    '(bar workspace-name window-number matches buffer-info buffer-position parrot selection-info)
    '(misc-info battery irc mu4e gnus github debug minor-modes buffer-encoding major-mode process))

  (doom-modeline-def-modeline 'package
    '(bar window-number package)
    '(misc-info major-mode process))

  (doom-modeline-def-modeline 'info
    '(bar window-number buffer-info info-nodes buffer-position parrot selection-info)
    '(misc-info buffer-encoding major-mode))

  (doom-modeline-def-modeline 'media
    '(bar window-number buffer-size buffer-info)
    '(misc-info media-info major-mode process
      ;; vcs
      ))

  (doom-modeline-def-modeline 'message
    '(bar window-number matches buffer-info-simple buffer-position word-count parrot selection-info)
    '(objed-state misc-info battery debug minor-modes input-method indent-info buffer-encoding major-mode))

  (doom-modeline-def-modeline 'pdf
    '(bar window-number matches buffer-info pdf-pages)
    '(misc-info major-mode process
      ;; vcs
      ))

  (doom-modeline-def-modeline 'org-src
    '(bar workspace-name window-number matches buffer-info-simple buffer-position word-count parrot selection-info)
    '(objed-state misc-info debug lsp minor-modes input-method indent-info buffer-encoding major-mode process checker)))

(defun pkg-doom-modeline/setup-mode ()
  (doom-modeline-mode +1)
  (remove-hook 'find-file-hook #'doom-modeline-update-vcs-text)
  (remove-hook 'find-file-hook #'doom-modeline-update-vcs-icon)
  (remove-hook 'after-save-hook #'doom-modeline-update-vcs-text)
  (remove-hook 'after-save-hook #'doom-modeline-update-vcs-icon))

(defun pkg-doom-modeline/-font-height-patch (&rest _args)
  "PATCH: Always return the specified `doom-modeline-height'."
  doom-modeline-height)

(my/package
  (doom-modeline :ref "fe9ee5a2a950f9ded10261a05a12adc577ae9e36")
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

  (setq doom-modeline-bar-width 1
        doom-modeline-buffer-encoding nil
        doom-modeline-buffer-modification-icon t
        doom-modeline-icon (display-graphic-p)
        doom-modeline-irc nil
        doom-modeline-major-mode-icon t
        doom-modeline-minor-modes nil
        doom-modeline-modal-icon nil
        doom-modeline-unicode-fallback nil)

  ;; Don't display version in the modeline.
  (setq doom-modeline-env-enable-elixir nil
        doom-modeline-env-enable-go nil
        doom-modeline-env-enable-perl nil
        doom-modeline-env-enable-python nil
        doom-modeline-env-enable-ruby nil
        doom-modeline-env-enable-rust nil)

  :config
  (pkg-doom-modeline/define-sections)

  ;; Set the modeline height to the minimum possible value.
  (advice-add #'doom-modeline--font-height :override #'pkg-doom-modeline/-font-height-patch))

(provide 'pkg-doom-modeline)
