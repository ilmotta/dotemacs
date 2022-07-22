;;; -*- lexical-binding: t; -*-

(my/package lsp-java
  :straight t
  :defer t
  :hook (java-mode-hook . lsp-deferred)
  :init
  (setq lsp-java-workspace-dir (concat my/local-dir "lsp-java-workspace/"))
  (setq lsp-java-workspace-cache-dir (concat temporary-file-directory "lsp-java-workspace/cache/"))

  ;; Autobuild is disabled because it consumes too many resources to be
  ;; enabled by default in bigger projects.
  (setq lsp-java-autobuild-enabled nil))

(provide 'pkg-lsp-java)
