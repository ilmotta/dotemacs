;;; -*- lexical-binding: t; -*-

(my/package flycheck-rust
  :straight t
  :defer t
  :hook (rust-mode-hook . flycheck-rust-setup))

(provide 'pkg-flycheck-rust)
