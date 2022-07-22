;;; -*- lexical-binding: t; -*-

(my/package nxml-mode
  :straight (:type built-in)
  :mode (rx "." (or "pom" "plist" "rss" "xml" "xsd" "xslt") string-end)
  :defer t
  :init
  ;; Typing a slash automatically completes the end-tag.
  (setq nxml-slash-auto-complete-flag t)
  (setq nxml-auto-insert-xml-declaration-flag nil))

(provide 'pkg-nxml-mode)
