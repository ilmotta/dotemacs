;;; -*- lexical-binding: t; -*-

(require 'lib-util)

(lib-util/pkg hl-line
  :ensure nil
  :hook ((conf-mode-hook
          dired-mode-hook
          org-agenda-mode-hook
          prog-mode-hook
          tablist--mode-hook
          text-mode-hook) . hl-line-mode)

  :init
  ;; Not having to render the hl-line overlay in multiple buffers offers a tiny
  ;; performance boost. I also don't need to see it in other buffers.
  (setq hl-line-sticky-flag nil
        global-hl-line-sticky-flag nil)

  ;; Emacs 28
  (setq hl-line-overlay-priority -50))

(provide 'pkg-hl-line)
