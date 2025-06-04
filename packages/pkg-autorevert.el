;;; -*- lexical-binding: t; -*-

(require 'lib-util)

(lib-util/pkg autorevert
  :ensure nil
  :init
  (setq auto-revert-verbose t
        auto-revert-stop-on-user-input nil
        auto-revert-use-notify nil))

(provide 'pkg-autorevert)
