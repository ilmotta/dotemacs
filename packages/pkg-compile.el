;;; -*- lexical-binding: t; -*-

(require 'lib-util)

(lib-util/pkg compile
  :ensure nil
  :init
  ;; Don't ask for the compilation command in the minibuffer. I often prefer to
  ;; set this variable in a directory locals file.
  (setq compilation-read-command nil))

(provide 'pkg-compile)
