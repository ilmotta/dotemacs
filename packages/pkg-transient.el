;;; -*- lexical-binding: t; -*-

(lib-util/pkg transient
  :elpaca nil
  :init
  (setq transient-values-file (file-name-concat my/cache-dir "transient/values.el")
        transient-levels-file (file-name-concat my/cache-dir "transient/levels.el")
        transient-history-file (file-name-concat my/cache-dir "transient/history.el"))

  ;; If non-nil, then the key binding of each suffix is colorized to indicate
  ;; whether it exits the transient state or not.
  ;;
  ;; Note: The colors look odd in most themes.
  (setq transient-semantic-coloring nil))

(provide 'pkg-transient)
