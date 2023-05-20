;;; -*- lexical-binding: t; -*-

;;; Commentary:
;; Dirvish is a minimalistic file manager based on dired. Dirvish does not
;; introduce any keybindings by default, see dirvish-mode-map for more details.
;;
;; Disabled @ 2023-02-04 because it's noticeably slower than Dired. Scrolling
;; lags, especially when `dired-hide-details-mode' is enabled, to the point
;; that's unusable.

;;; Code:

(my/package dirvish
  :elpaca (:ref "4b63cd2e5ba994f8e674388db7035de1a8f0343f")
  :disabled t
  :defer t

  :init
  ;; (setq dirvish-body-fontsize-increment 0.0)
  (setq dirvish-cache-dir (concat my/cache-dir "dirvish/"))
  (setq dirvish-attributes '(all-the-icons))

  ;; The default is too low.
  (setq dirvish-redisplay-debounce 0.05)

  ;; My cursor stands out and helps me see where I am.
  (setq dirvish-hide-cursor nil)

  :config
  (dirvish-override-dired-mode))

(provide 'pkg-dirvish)
