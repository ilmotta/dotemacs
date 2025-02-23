;;; -*- lexical-binding: t; -*-

(require 'lib-util)

(lib-util/pkg transient
  :elpaca (:host github
           :repo "magit/transient"
           :ref "d6b52b6eb7655d538b410ae60f85fbaa4d58a608") ; 0.7.8
  :init
  (setq transient-values-file (file-name-concat my/cache-dir "transient/values.el")
        transient-levels-file (file-name-concat my/cache-dir "transient/levels.el")
        transient-history-file (file-name-concat my/cache-dir "transient/history.el"))

  ;; If non-nil, then the key binding of each suffix is colorized to indicate
  ;; whether it exits the transient state or not.
  ;;
  ;; Note: The colors look odd in most themes.
  (setq transient-semantic-coloring nil)

  :config
  ;; "C-g" is the default binding for such commands now, but Transient's
  ;; predecessor Magit-Popup used "q" instead.
  (transient-bind-q-to-quit))

(provide 'pkg-transient)
