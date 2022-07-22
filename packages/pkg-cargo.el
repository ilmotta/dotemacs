;;; -*- lexical-binding: t; -*-

(my/package cargo
  :straight t
  :defer t
  :hook (rust-mode-hook . cargo-minor-mode))

(provide 'pkg-cargo)
