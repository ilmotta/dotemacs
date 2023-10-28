;;; -*- lexical-binding: t; -*-

(require 'lib-util)

(lib-util/pkg eglot
  :elpaca (:host github
           :repo "joaotavora/eglot"
           :ref "8b5532dd32b25276c1857508030b207f765ef9b6")
  :init
  (my/general-mode-def
    :keymaps '(clojure-mode-map
               go-mode-map
               go-ts-mode-map)
    "c a" #'eglot-code-actions
    "c f d" #'eglot-find-typeDefinition
    "c f i" #'eglot-find-implementation
    "c f l" #'eglot-find-declaration
    "c f r" #'xref-find-references
    "c r" #'eglot-rename)

  ;; Don't block while connecting.
  (setq eglot-sync-connect nil)

  ;; Don't auto-shutdown when the last buffer is killed. I often killed the
  ;; buffer, but I'll reopen another one in the same project.
  (setq eglot-autoshutdown nil)

  (setq eglot-autoreconnect nil)
  (setq eglot-connect-timeout 30)
  (setq eglot-events-buffer-size 2000000)
  (setq eglot-stay-out-of '("company")))

(provide 'pkg-eglot)
