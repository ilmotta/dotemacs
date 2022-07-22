;;; -*- lexical-binding: t; -*-

;;; Commentary:
;; Manage init system services like GNU Shepherd and systemd.

;;; Code:

(my/package daemons
  :when my/linux?
  :straight t
  :defer t
  :commands (daemons
             daemons-disable
             daemons-enable
             daemons-reload
             daemons-restart
             daemons-start
             daemons-stop)
  :init
  (setq daemons-list-fill-frame t
        daemons-systemd-is-user t))

(provide 'pkg-daemons)
