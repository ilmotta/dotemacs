;;; -*- lexical-binding: t; -*-

;;; Code:

(defun pkg-consult/-consult-line (original-fn &rest args)
  (let ((consult-preview-key 'any))
    (apply original-fn args)))

(defun pkg-consult/-consult-outline (original-fn &rest args)
  (let ((consult-preview-key 'any))
    (apply original-fn args)))

(defun pkg-consult/completion-in-region-function (&rest args)
  "Use `consult-completion-in-region' if Vertico is enabled.
Otherwise use the default `completion--in-region' function.

There is a technical limitation of `consult-completion-in-region'
in combination with Lsp-mode or Eglot. The Lsp server relies on
the input at point, in order to generate refined candidate
strings. Since the completion is transferred from the original
buffer to the minibuffer, the server does not receive the updated
input. LSP completion works with Corfu or Company though, which
perform the completion directly in the original buffer."
  (apply (if (bound-and-true-p vertico-mode)
             #'consult-completion-in-region
           #'completion--in-region)
         args))

(defun pkg-consult/ripgrep-dwim ()
  "Search with rg using symbol at point as initial input."
  (interactive)
  (if-let ((proj (project-current)))
      (funcall #'consult-ripgrep (project-root proj) (thing-at-point 'symbol))
    (funcall #'consult-ripgrep default-directory (thing-at-point 'symbol))))

(my/package
  (consult :ref "7c514c0a2414347c4cd0482a691371625a8a1c53")
  :defer t

  :init
  (general-def
    :keymaps 'eshell-hist-mode-map
    :states '(insert emacs)
    [remap eshell-previous-matching-input] #'consult-history)

  (general-def
    :keymaps 'my/keys-mode-map
    [remap apropos-command]                     #'consult-apropos
    [remap bookmark-jump]                       #'consult-bookmark
    [remap copy-to-register]                    #'consult-register
    [remap execute-extended-command-for-buffer] #'consult-mode-command
    [remap imenu]                               #'consult-imenu
    [remap list-buffers]                        #'consult-buffer
    [remap project-switch-to-buffer]            #'consult-project-buffer
    [remap switch-to-buffer-other-frame]        #'consult-buffer-other-frame
    [remap switch-to-buffer-other-window]       #'consult-buffer-other-window
    [remap switch-to-buffer]                    #'consult-buffer
    [remap yank-pop]                            #'consult-yank-pop
    "C-x /"                                     #'consult-line)

  (general-def
    :keymaps '(outline-minor-mode-map eshell-mode-map)
    :prefix "C-c h"
    "/" #'consult-outline)

  (general-def
    :keymaps 'org-mode-map
    :prefix "C-c h"
    "/" #'consult-org-heading)

  (general-def
    :keymaps 'project-prefix-map
    "s s" #'consult-ripgrep
    "s ." #'pkg-consult/ripgrep-dwim)

  (setq xref-show-xrefs-function #'consult-xref
        xref-show-definitions-function #'consult-xref)

  (setq consult-buffer-sources '(consult--source-hidden-buffer
                                 consult--source-buffer
                                 consult--source-recent-file
                                 consult--source-bookmark
                                 consult--source-project-buffer
                                 consult--source-project-recent-file))

  (setq completion-in-region-function #'pkg-consult/completion-in-region-function)

  (setq consult-preview-key (list (kbd "M-k") (kbd "M-j")))
  (setq consult-narrow-key ">")
  (setq consult-widen-key "<")
  (setq consult-line-numbers-widen t)

  (setq consult-async-input-debounce 0.25)
  (setq consult-async-input-throttle 0.25)
  (setq consult-async-min-input 3)
  (setq consult-async-refresh-delay 0.1)
  (setq consult-async-default-split nil)

  ;; Default is 'perl, which adds a hashtag to the initial input. This allows
  ;; two-level filtering, i.e. first level is sent to the external program
  ;; asynchronously and the second level uses the built-in Emacs mechanism (e.g.
  ;; orderless).
  (setq consult-async-split-style 'perl)

  :config
  (consult-customize
   consult-focus-lines
   consult-imenu
   consult-imenu-multi
   consult-keep-lines
   consult-org-heading
   consult-outline
   :preview-key 'any

   ;; Fast scrolling with a low debounce (e.g. 100ms) will prevent needlessly
   ;; previewing buffers.
   consult-buffer
   :preview-key (append '(:debounce 0.05) consult-preview-key))

  (advice-add #'consult-line :around #'pkg-consult/-consult-line)
  (advice-add #'consult-outline :around #'pkg-consult/-consult-outline))

(provide 'pkg-consult)
