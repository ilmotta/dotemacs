;;; -*- lexical-binding: t; -*-

(require 'lib-util)

(lib-util/pkg ispell
  :elpaca nil
  :config
  (when-let* ((dir (getenv "ASPELL_DICT_DIR")))
    (setq ispell-extra-args `("--dict-dir" ,dir)))
  (add-to-list 'ispell-skip-region-alist '(":\\(PROPERTIES\\|LOGBOOK\\):" . ":END:"))
  (add-to-list 'ispell-skip-region-alist '("#\\+BEGIN_SRC" . "#\\+END_SRC")))

(provide 'pkg-ispell)
