;;; -*- lexical-binding: t; -*-

;;; Commentary:
;; Practice touch/speed typing in Emacs.

;;; Code:

(my/package speed-type
  :elpaca (:ref "11a8bd33711711fb5e22d93ac2ed950e4a2e76fc")
  :defer t
  :commands (speed-type-region
             speed-type-text
             speed-type-top-100
             speed-type-top-1000)
  :init
  (setq speed-type-gb-dir (concat my/cache-dir "speed-type/")))

(provide 'pkg-speed-type)
