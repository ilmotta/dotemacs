;;; -*- lexical-binding: t; -*-

(lib-util/pkg lsp-java
  :elpaca (:ref "de2d89814fecb9bae825baa7028c5cd8b32b9b8f")
  :defer t
  :hook (java-mode-hook . lsp-deferred)
  :init
  (setq lsp-java-workspace-dir (concat my/local-dir "lsp-java-workspace/"))
  (setq lsp-java-workspace-cache-dir (concat temporary-file-directory "lsp-java-workspace/cache/"))

  ;; Autobuild is disabled because it consumes too many resources to be
  ;; enabled by default in bigger projects.
  (setq lsp-java-autobuild-enabled nil))

(provide 'pkg-lsp-java)
