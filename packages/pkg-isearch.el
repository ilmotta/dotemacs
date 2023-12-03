;;; -*- lexical-binding: t; -*-

(require 'lib-util)

(lib-util/pkg isearch
  :elpaca nil
  :init
  ;; Isearch has been improved in Emacs 27 and can now display match numbers in
  ;; the modeline.
  ;;
  ;; Highlights matches in the full buffer. It is useful in combination with
  ;; `lazy-highlight-cleanup' customized to nil to leave matches highlighted in
  ;; the whole buffer after exiting isearch.
  (setq isearch-lazy-count t
        isearch-allow-scroll t
        lazy-highlight-initial-delay 0
        lazy-highlight-buffer t
        lazy-highlight-cleanup t
        lazy-highlight-buffer-max-at-a-time 10))

(provide 'pkg-isearch)
