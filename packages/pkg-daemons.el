;;; -*- lexical-binding: t; -*-

;;; Commentary:
;; Manage init system services like GNU Shepherd and systemd.

;;; Code:

(require 'lib-util)

(lib-util/pkg daemons
  :ensure (:ref "e18e84ccc13101f1609c213029cf011ae0ad1178")
  :when my/linux?
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
