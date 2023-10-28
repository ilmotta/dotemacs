;;; -*- lexical-binding: t; -*-

(require 'lib-util)

(lib-util/pkg request
  :elpaca (:ref "fe567ec0222a1ba658866697a9e7fb6b63d71ff7")
  :defer t
  :init
  (setq request-storage-directory (concat my/cache-dir "request")))

(provide 'pkg-request)
