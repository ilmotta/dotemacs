;;; -*- lexical-binding: t; -*-

(my/package request
  :straight t
  :defer t
  :init
  (setq request-storage-directory (concat my/cache-dir "request")))

(provide 'pkg-request)
