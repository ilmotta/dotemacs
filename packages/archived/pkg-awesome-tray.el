;;; -*- lexical-binding: t; -*-

;;; Commentary:
;; Interesting approach, but it has weird UI issues (as of 2022-08-07).

;;; Code:

(my/package awesome-tray
  :straight (:host github :repo "manateelazycat/awesome-tray")
  :defer t
  :hook (after-init-hook . awesome-tray-mode)
  :init
  (setq awesome-tray-active-modules
        '("location"
          "file-path"
          "buffer-name"
          "buffer-read-only"
          "mode-name"))

  (setq awesome-tray-buffer-name-buffer-changed t)
  (setq awesome-tray-buffer-name-buffer-changed-style "*")
  (setq awesome-tray-buffer-name-max-length 30)
  (setq awesome-tray-buffer-read-only-style "🔒")
  (setq awesome-tray-evil-show-cursor-count nil)
  (setq awesome-tray-evil-show-macro nil)
  (setq awesome-tray-evil-show-mode nil)
  (setq awesome-tray-file-name-max-length 30)
  (setq awesome-tray-file-path-show-filename nil)
  (setq awesome-tray-git-show-status nil)
  (setq awesome-tray-minibuffer nil)
  (setq awesome-tray-mode-line-active-color "royal blue")
  (setq awesome-tray-mode-line-height 0.1)
  (setq awesome-tray-refresh-idle-delay 5)
  (setq awesome-tray-separator "  ")
  (setq awesome-tray-update-interval 1))

(provide 'pkg-awesome-tray)
