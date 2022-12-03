;;; -*- lexical-binding: t -*-

(add-to-list 'load-path (file-truename (concat user-emacs-directory "packages/")))
(add-to-list 'load-path (file-truename (concat user-emacs-directory "lisp/")))

(require 'cl-macs)
(require 'transient)
(require 'seq)
(require 'lib-util)

;; Load core modules.
(dolist (file '("defaults.el"
                "package-manager.el"
                "built-in-libs.el"))
  (load (concat user-emacs-directory "core/" file) nil 'no-message))

(defun my/load-extra-libs ()
  (require 'lib-adb)
  (require 'lib-backup)
  (require 'lib-fs)
  (require 'lib-metabase)
  (require 'lib-media)
  (require 'lib-pdf)
  (require 'lib-system))

(add-hook 'after-init-hook #'my/load-extra-libs)

;;; Emacs Lisp development libraries
(my/with-packages
 pkg-dash
 pkg-f
 pkg-posframe
 pkg-promise
 pkg-request
 pkg-s
 pkg-shrink-path
 pkg-ts)

;;; Core packages
(my/with-packages
 pkg-general
 pkg-org ; Org must be loaded before the bundled org-mode one.
 pkg-evil
 pkg-evil-collection
 pkg-magit
 pkg-xclip)

;;; Built-in packages
(my/with-packages
 pkg-dired
 pkg-eshell
 pkg-proced
 pkg-project
 pkg-recentf
 pkg-savehist
 pkg-saveplace
 pkg-xref)

;;; Completion systems
(my/with-packages
 pkg-consult
 pkg-corfu
 pkg-embark
 pkg-orderless
 pkg-vertico)

;;; UI packages
(my/with-packages
 pkg-all-the-icons
 pkg-all-the-icons-dired
 pkg-anzu
 pkg-ctrlf
 pkg-dimmer
 pkg-diredfl
 pkg-doom-modeline
 pkg-doom-themes
 pkg-eros
 pkg-goto-addr
 pkg-helpful
 pkg-hide-mode-line
 pkg-highlight-parentheses
 pkg-hl-line
 pkg-idle-highlight-mode
 pkg-kind-icon
 pkg-marginalia
 pkg-page-break-lines
 pkg-paren-face
 pkg-pulsar
 pkg-xterm-color)

;;; Packages that enhance text editing
(my/with-packages
 pkg-aggressive-indent
 pkg-apheleia
 pkg-drag-stuff
 pkg-emmet-mode
 pkg-evil-cleverparens
 pkg-evil-matchit
 pkg-evil-nerd-commenter
 pkg-evil-smartparens
 pkg-evil-surround
 pkg-flycheck
 pkg-flyspell-correct
 pkg-quickrun
 pkg-smartparens
 pkg-tempo
 pkg-undo-fu)

;;; File/language modes
(my/with-packages
 pkg-cargo
 pkg-cider
 pkg-clj-refactor
 pkg-clojure-mode
 pkg-csv-mode
 pkg-dockerfile-mode
 pkg-elisp-mode
 pkg-flycheck-clj-kondo
 pkg-flycheck-ledger
 pkg-flycheck-rust
 pkg-geiser
 pkg-git-modes
 pkg-gnuplot
 pkg-go-mode
 pkg-graphql-mode
 pkg-groovy-mode
 pkg-haskell-mode
 pkg-js2-mode
 pkg-json-mode
 pkg-kbd-mode
 pkg-kotlin-mode
 pkg-ledger-mode
 pkg-lua-mode
 pkg-markdown-mode
 pkg-nix-mode
 pkg-nxml-mode
 pkg-php-mode
 pkg-plantuml-mode
 pkg-rust-mode
 pkg-terraform-mode
 pkg-tree-sitter
 pkg-tree-sitter-langs
 pkg-tsi
 pkg-typescript-mode
 pkg-vimrc-mode
 pkg-web-mode
 pkg-yaml-mode)

;;; LSP (Language Server Protocol)
(my/with-packages
 pkg-dap-mode
 pkg-lsp-java
 pkg-lsp-ui
 pkg-lsp-mode)

;;; Org mode
(my/with-packages
 pkg-evil-org
 pkg-htmlize
 pkg-ob-async
 pkg-org-capture
 pkg-org-cliplink
 pkg-org-download
 pkg-org-make-toc
 pkg-org-modern
 pkg-org-roam)

;;; Miscellaneous packages
(my/with-packages
 pkg-ace-window
 pkg-browse-at-remote
 pkg-command-log-mode
 pkg-consult-dir
 pkg-daemons
 pkg-deadgrep
 pkg-devdocs-lookup
 pkg-dired-du
 pkg-explain-pause-mode
 pkg-hnreader
 pkg-openwith
 pkg-pdf-tools
 pkg-popper
 pkg-speed-type
 pkg-sudo-edit
 pkg-svg-clock
 pkg-synosaurus
 pkg-timer-revert
 pkg-trashed
 pkg-visual-fill-column
 pkg-vterm
 pkg-wgrep
 pkg-wordnut)

;;; Internal packages
(my/with-packages
 pkg-outline)

;;; Final pieces of the puzzle
(my/with-packages
 pkg-daemon
 pkg-theme)

(require 'keybindings)
