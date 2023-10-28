;;; -*- lexical-binding: t; -*-

(lib-util/pkg image-dired
  :elpaca nil
  :init
  (setq image-dired-thumb-size 150
        image-dired-thumb-margin 2
        ;; No border around thumbnails
        image-dired-thumb-relief 0)

  ;; Where to store image caches.
  (setq image-dired-dir (concat my/cache-dir "image-dired/")
        image-dired-db-file (concat image-dired-dir "db.el")
        image-dired-gallery-dir (concat image-dired-dir "gallery/")
        image-dired-temp-image-file (concat image-dired-dir "temp-image")
        image-dired-temp-rotate-image-file (concat image-dired-dir "temp-rotate-image")))

(provide 'pkg-image-dired)
