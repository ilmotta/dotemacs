;;; -*- lexical-binding: t; -*-

(lib-util/pkg tramp
  :elpaca nil
  :init
  ;; Value of TERM environment variable for logging in to remote host.
  (setq tramp-terminal-type "tramp")

  ;; Set from 0 to 11 (more verbose). Level 1 means only errors will be logged.
  ;; Default is 3.
  (setq tramp-verbose 1)

  ;; Set path to file which keeps connection history.
  (setq tramp-persistency-file-name (file-name-concat my/cache-dir "tramp")))

(provide 'pkg-tramp)
