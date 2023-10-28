;;; -*- lexical-binding: t -*-

(add-to-list 'load-path (file-truename (concat user-emacs-directory "packages/")))
(add-to-list 'load-path (file-truename (concat user-emacs-directory "lisp/")))

(require 'cl-macs)
(require 'cl-lib)
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
  (require 'lib-bak)
  (require 'lib-fs)
  (require 'lib-metabase)
  (require 'lib-media)
  (require 'lib-pdf)
  (require 'lib-sys))

(add-hook 'after-init-hook #'my/load-extra-libs)

;;; Core packages
(my/with-packages
 pkg-general               ; Sync load before all other packages.
 pkg-org                   ; Org must be loaded before the bundled org-mode one.
 pkg-evil
 pkg-evil-collection
 pkg-magit
 pkg-undo-fu
 pkg-xclip)

;;; Built-in packages
(my/with-packages
 pkg-dired
 pkg-eldoc
 pkg-emacs
 pkg-eshell
 pkg-goto-addr
 pkg-hl-line
 pkg-image-dired
 pkg-outline
 pkg-proced
 pkg-project
 pkg-recentf
 pkg-tab-bar
 pkg-tempo
 pkg-theme
 pkg-tramp
 pkg-transient
 pkg-winner)

;;; Emacs Lisp development libraries
(my/with-packages
 pkg-dash
 pkg-f
 pkg-plz
 pkg-posframe
 pkg-promise
 pkg-request
 pkg-s
 pkg-graphql
 pkg-shrink-path
 pkg-ts)

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
 pkg-battery
 pkg-ctrlf
 pkg-dimmer
 pkg-diredfl
 pkg-doom-modeline
 pkg-doom-themes
 pkg-eros
 pkg-helpful
 pkg-hide-mode-line
 pkg-highlight-parentheses
 pkg-idle-highlight-mode
 pkg-kind-icon
 pkg-magit-delta
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
 pkg-xref)

;;; File/language modes
(my/with-packages
 pkg-cargo
 pkg-cider
 pkg-clj-refactor
 pkg-clojure-mode
 pkg-csv-mode
 pkg-dockerfile-mode
 pkg-flycheck-clj-kondo
 pkg-flycheck-ledger
 pkg-flycheck-rust
 pkg-geiser
 pkg-git-modes
 pkg-gnuplot
 pkg-go-mode
 pkg-gotest
 pkg-graphql-mode
 pkg-groovy-mode
 pkg-haskell-mode
 pkg-js2-mode
 pkg-just-mode
 pkg-kbd-mode
 pkg-kotlin-mode
 pkg-ledger-mode
 pkg-lua-mode
 pkg-markdown-mode
 pkg-nim-mode
 pkg-nix-mode
 pkg-php-mode
 pkg-plantuml-mode
 pkg-protobuf-mode
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
 pkg-eglot
 pkg-dap-mode
 pkg-lsp-java
 pkg-lsp-mode
 pkg-lsp-ui)

;;; Org mode
(my/with-packages
 pkg-evil-org
 pkg-htmlize
 pkg-ob-async
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
 pkg-dirvish
 pkg-explain-pause-mode
 pkg-golden-ratio
 pkg-hnreader
 pkg-openwith
 pkg-pdf-tools
 pkg-popper
 pkg-speed-type
 pkg-status-mobile
 pkg-sudo-edit
 pkg-svg-clock
 pkg-synosaurus
 pkg-timer-revert
 pkg-trashed
 pkg-visual-fill-column
 pkg-vterm
 pkg-wgrep
 pkg-wordnut
 pkg-zones)

(require 'keybindings)
