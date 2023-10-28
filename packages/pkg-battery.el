;;; -*- lexical-binding: t; -*-

(lib-util/pkg battery
  :elpaca nil
  :init
  (setq battery-mode-line-limit 99
        battery-update-interval 60
        battery-load-low 20
        battery-load-critical 10))

(provide 'pkg-battery)
