;;; -*- lexical-binding: t; -*-

;;; Monkey patches

(defun pkg-xref/pop-to-location (_original-fn item &optional action)
  "Go to the location of ITEM and display the buffer.
ACTION controls how the buffer is displayed:
  nil      -- `switch-to-buffer'
  `window' -- `pop-to-buffer' (other window)
  `frame'  -- `pop-to-buffer' (other frame)
If SELECT is non-nil, select the target window.

MONKEY PATCH:

Don't pass t to `pop-to-buffer' action argument, so that the
command respects buffer display actions."
  (let* ((marker (save-excursion
                   (xref-location-marker (xref-item-location item))))
         (buf (marker-buffer marker)))
    (cl-ecase action
      ((nil)  (switch-to-buffer buf))
      (window (pop-to-buffer buf))
      (frame  (let ((pop-up-frames t)) (pop-to-buffer buf t))))
    (xref--goto-char marker))
  (let ((xref--current-item item))
    (run-hooks 'xref-after-jump-hook)))

(defun pkg-xref/pop-marker-stack-advice-around (original-fn &rest args)
  (let ((display-buffer-overriding-action (when my/reuse-buffer-window-p
                                            '((display-buffer-reuse-window)))))
    (apply original-fn args)))

;;; Autoloads

;;;###autoload
(defun pkg-xref/find-references-dwim ()
  (interactive)
  (if-let (thing (thing-at-point 'symbol))
      (let ((xref-prompt-for-identifier nil))
        (call-interactively #'xref-find-references))
    (let ((xref-prompt-for-identifier t))
      (call-interactively #'xref-find-references))))

;;;###autoload
(defun pkg-xref/find-definitions ()
  "Find xref definitions with display action set to 'window, so
that `xref-pop-to-location' uses `pop-to-buffer', which in turn
respects display buffer actions."
  (interactive)
  (my/with-buffer-reuse-window
   (cond ((bound-and-true-p lsp-mode)
          (lsp-find-definition :display-action 'window))
         (:default
          (let ((thing (thing-at-point 'symbol)))
            ;; Uses `display-action' as 'window.
            (xref-find-definitions-other-window thing))))))

;;; Package

(my/package xref
  :straight (:type built-in)
  :defer t

  :hook (xref-after-jump-hook . recenter)
  :hook (xref-after-return-hook . recenter)
  :hook (xref-after-jump-hook . xref-pulse-momentarily)
  :hook (xref-after-return-hook . xref-pulse-momentarily)

  :init
  (general-def
    [remap xref-find-references] '(pkg-xref/find-references-dwim :properties (:jump t))
    [remap xref-find-definitions] '(pkg-xref/find-definitions :properties (:jump t)))

  (setq xref-search-program 'ripgrep)
  (setq xref-auto-jump-to-first-definition nil)
  (setq xref-auto-jump-to-first-xref nil)

  :config
  (advice-add #'xref-pop-to-location :around #'pkg-xref/pop-to-location)
  (advice-add #'xref-pop-marker-stack :around #'pkg-window/display-buffer-reuse-window))

(provide 'pkg-xref)
