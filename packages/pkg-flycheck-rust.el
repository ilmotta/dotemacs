;;; -*- lexical-binding: t; -*-

(require 'lib-util)

(lib-util/pkg flycheck-rust
  :elpaca (:ref "a139cd53c5062697e9ed94ad80b803c37d999600")
  :defer t
  :hook (rust-mode-hook . flycheck-rust-setup))

(provide 'pkg-flycheck-rust)
