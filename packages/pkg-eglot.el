;;; -*- lexical-binding: t; -*-

(my/package eglot
  :elpaca nil
  :init
  ;; Don't block while connecting.
  (setq eglot-sync-connect nil)
  (setq eglot-autoshutdown t)
  (setq eglot-autoreconnect nil)
  (setq eglot-connect-timeout 30)
  (setq eglot-events-buffer-size 2000000)
  (setq eglot-stay-out-of '("company")))

(provide 'pkg-eglot)
