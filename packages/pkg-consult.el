;;; -*- lexical-binding: t; -*-

;;; Code:

(require 'lib-util)

(defvar pkg-consult/-ripgrep-dwim-previous-query nil
  "Stores the latest query when calling `pkg-consult/ripgrep-dwim'.")

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
  (let ((thing (thing-at-point 'symbol))
        (dir (if-let ((proj (project-current)))
                 (project-root proj)
               default-directory)))
    (setq pkg-consult/-ripgrep-dwim-previous-query thing)
    (consult-ripgrep dir thing)))

(defun pkg-consult/ripgrep-dwim-previous ()
  "Searches with rg.
Uses initial input from previous call to
`pkg-consult/ripgrep-dwim'."
  (interactive)
  (let ((dir (if-let ((proj (project-current)))
                 (project-root proj)
               default-directory)))
    (consult-ripgrep dir pkg-consult/-ripgrep-dwim-previous-query)))

(lib-util/pkg consult
  :elpaca (:host github
           :repo "minad/consult"
           :ref "ce38dd037769ccba93e7b854ab9b0cc0eced84ee")
  :defer t

  :init
  (general-def
    :keymaps 'eshell-hist-mode-map
    :states '(insert emacs)
    [remap eshell-previous-matching-input] #'consult-history)

  (general-def
    :keymaps 'my/keys-mode-map
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
    [remap evil-search-forward]                 #'consult-line)

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
    "s ." #'pkg-consult/ripgrep-dwim
    "s l" #'pkg-consult/ripgrep-dwim-previous)

  (setq xref-show-xrefs-function #'consult-xref
        xref-show-definitions-function #'consult-xref)

  (setq consult-buffer-sources '(consult--source-hidden-buffer
                                 consult--source-buffer
                                 consult--source-recent-file
                                 consult--source-bookmark
                                 consult--source-project-buffer
                                 consult--source-project-recent-file))

  (setq completion-in-region-function #'pkg-consult/completion-in-region-function)

  (setq consult-preview-key (list my/consult-preview-and-previous-line-key
                                  my/consult-preview-and-next-line-key))
  (setq consult-narrow-key ">")
  (setq consult-widen-key "<")
  (setq consult-line-numbers-widen t)

  (setq consult-async-input-debounce 0.25)
  (setq consult-async-input-throttle 0.25)
  (setq consult-async-min-input 3)
  (setq consult-async-refresh-delay 0.1)

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
   consult-ripgrep
   :preview-key (append '(:debounce 0.05) consult-preview-key))

  (advice-add #'consult-line :around #'pkg-consult/-consult-line)
  (advice-add #'consult-outline :around #'pkg-consult/-consult-outline))

(provide 'pkg-consult)
