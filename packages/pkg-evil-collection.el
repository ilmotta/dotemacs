;;; -*- lexical-binding: t; -*-

(require 'lib-util)

(defun pkg-evil-collection/setup ()
  (when my/evil-p
    (require 'evil-collection)
    (evil-collection-init)))

(lib-util/pkg evil-collection
  :elpaca (:ref "b7a75062a600b1b1d2ba51a1e3ac1ec331d19fff")
  :defer t
  :init
  (add-hook 'elpaca-after-init-hook #'pkg-evil-collection/setup -9998)

  ;; Precisely control which modes are affected by `evil-collection'.
  (setq evil-collection-mode-list
        '(2048-game
          ;; ag
          ;; alchemist
          anaconda-mode
          apropos
          arc-mode
          atomic-chrome
          auto-package-update
          beginend
          ;; bluetooth
          bm
          bookmark
          (buff-menu "buff-menu")
          calc
          calendar
          cider
          cmake-mode
          comint
          ;; company
          compile
          consult
          ;; corfu
          (custom cus-edit)
          cus-theme
          dashboard
          daemons
          deadgrep
          debbugs
          debug
          devdocs
          dictionary
          diff-hl
          diff-mode
          dired
          dired-sidebar
          disk-usage
          distel
          doc-view
          docker
          ebib
          ebuku
          edbi
          edebug
          ediff
          eglot
          explain-pause-mode
          eldoc
          elfeed
          elisp-mode
          elisp-refs
          elisp-slime-nav
          embark
          emms
          epa
          ert
          eshell
          eval-sexp-fu
          evil-mc
          eww
          fanyi
          ;; finder
          flycheck
          flymake
          forge
          free-keys
          geiser
          ggtags
          git-timemachine
          gnus
          go-mode
          grep
          guix
          hackernews
          helm
          help
          helpful
          hg-histedit
          hungry-delete
          ibuffer
          image
          image-dired
          image+
          imenu
          imenu-list
          (indent "indent")
          indium
          info
          ivy
          js2-mode
          leetcode
          lispy
          log-edit
          log-view
          lsp-ui-imenu
          lua-mode
          kotlin-mode
          macrostep
          man
          (magit magit-repos magit-submodule)
          magit-section
          magit-todos
          markdown-mode
          monky
          mpc
          mpdel
          mu4e
          mu4e-conversation
          neotree
          newsticker
          notmuch
          nov
          omnisharp
          org
          org-present
          org-roam
          osx-dictionary
          ;; outline
          p4
          (package-menu package)
          pass
          (pdf pdf-view)
          popup
          proced
          (process-menu simple)
          prodigy
          profiler
          python
          quickrun
          racer
          racket-describe
          realgud
          reftex
          replace
          restclient
          rg
          ripgrep
          rjsx-mode
          robe
          rtags
          ruby-mode
          scheme
          scroll-lock
          ;; selectrum
          sh-script
          shortdoc
          simple
          simple-mpc
          slime
          sly
          snake
          so-long
          speedbar
          tab-bar
          tablist
          tabulated-list
          tar-mode
          ;; telega
          (term term ansi-term multi-term)
          tetris
          thread
          tide
          timer-list
          ;; transmission
          trashed
          tuareg
          typescript-mode
          vc-annotate
          vc-dir
          vc-git
          vdiff
          vertico
          view
          vlf
          vterm
          vundo
          w3m
          wdired
          wgrep
          ;; which-key
          woman
          xref
          xwidget
          yaml-mode
          ;; youtube-dl
          ;; zmusic
          (ztree ztree-diff ztree-dir)))

  ;; Disable unimpaired mode because I don't use it.
  (setq evil-collection-want-unimpaired-p nil)

  ;; Enable company-tng behavior, i.e. You'll be able to press TAB to cycle over
  ;; completions and automatically insert the selection in the buffer.
  (setq evil-collection-company-use-tng t)

  (setq evil-collection-outline-maps '(outline-minor-mode-map)))

(provide 'pkg-evil-collection)
