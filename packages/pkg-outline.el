;;; -*- lexical-binding: t; -*-

(defun pkg-outline/recenter (&rest _args)
  (recenter))

(defun pkg-outline/delete-tree ()
  "Delete tree at point."
  (interactive)
  (let ((start nil)
        (end nil))
    (save-excursion
      (outline-back-to-heading)
      (setq start (point))
      (outline-end-of-subtree)
      (setq end (point)))
    (delete-region start end)
    (delete-region (line-beginning-position) (1+ (line-end-position)))))

(defun pkg-outline/next-hide-other-heading ()
  (interactive)
  (call-interactively #'outline-next-heading)
  (call-interactively #'outline-hide-other))

(defun pkg-outline/previous-hide-other-heading ()
  (interactive)
  (call-interactively #'outline-previous-heading)
  (call-interactively #'outline-hide-other))

(lib-util/pkg outline
  :elpaca nil
  :init
  (setq outline-minor-mode-highlight 'override)

  ;; Because C-i is translated as <TAB>, when the cursor is on an outline, it'll
  ;; toggle the subtree's visibility instead of `evil-jump-forward'.
  (setq outline-minor-mode-cycle t)

  :config
  (advice-add #'outline-backward-same-level :after #'pkg-outline/recenter)
  (advice-add #'outline-forward-same-level :after #'pkg-outline/recenter)
  (advice-add #'outline-next-visible-heading :after #'pkg-outline/recenter)
  (advice-add #'outline-previous-visible-heading :after #'pkg-outline/recenter)
  (advice-add #'outline-cycle-buffer :after #'pkg-outline/recenter)
  (advice-add #'outline-hide-other :after #'pkg-outline/recenter))

(provide 'pkg-outline)
