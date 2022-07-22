;;; -*- lexical-binding: t; -*-

(defun pkg-outline/recenter (&rest _args)
  (recenter))

;;;###autoload
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

;;;###autoload
(defun pkg-outline/next-hide-other-heading ()
  (interactive)
  (call-interactively #'outline-next-heading)
  (call-interactively #'outline-hide-other))

;;;###autoload
(defun pkg-outline/previous-hide-other-heading ()
  (interactive)
  (call-interactively #'outline-previous-heading)
  (call-interactively #'outline-hide-other))

(my/package outline
  :straight (:type built-in)
  :defer t

  :init
  (when my/evil-p
    (general-def
      :keymaps '(outline-mode-map outline-minor-mode-map)
      :states 'normal
      :predicate '(outline-on-heading-p)
      "C-j" '(outline-next-visible-heading :properties (:jump t))
      "C-k" '(outline-previous-visible-heading :properties (:jump t))
      "C-M-j" '(pkg-outline/next-hide-other-heading :properties (:jump t))
      "C-M-k" '(pkg-outline/previous-hide-other-heading :properties (:jump t)))

    (general-def
      :keymaps '(outline-mode-map outline-minor-mode-map)
      :states 'normal
      "z /" #'consult-outline
      "z b" #'outline-cycle-buffer
      "z j" '(outline-next-visible-heading :properties (:jump t))
      "z k" '(outline-previous-visible-heading :properties (:jump t))
      "z p" #'outline-hide-other
      "z u" '(outline-up-heading :properties (:repeat t :jump t)))

    (general-def
      :keymaps '(outline-mode-map outline-minor-mode-map)
      :states 'normal
      :predicate '(outline-on-heading-p)
      "z a" #'outline-toggle-children
      "z c" #'outline-hide-entry
      "z C" #'outline-hide-subtree
      "z d" '(pkg-outline/delete-tree :properties (:repeat t))
      "z J" '(outline-move-subtree-down :properties (:repeat t))
      "z K" '(outline-move-subtree-up :properties (:repeat t))
      "z o" #'outline-show-entry
      "z O" #'outline-show-subtree))

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
