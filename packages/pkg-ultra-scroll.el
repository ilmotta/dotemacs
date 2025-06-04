;;; -*- lexical-binding: t; -*-

(require 'lib-util)

(lib-util/pkg ultra-scroll
  :ensure (:host github
           :repo "jdtsmith/ultra-scroll"
           :rev "2c517bf9b61bf432f706ff8a585ba453c7476be2")
  :hook (elpaca-after-init-hook . ultra-scroll-mode)
  :init
  (setq scroll-conservatively 101 ; important!
        scroll-margin 0))

(provide 'pkg-ultra-scroll)
