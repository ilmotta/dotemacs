;;; -*- lexical-binding: t; -*-

(require 'lib-util)

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

;;; Interactive

(defun pkg-xref/find-references-dwim ()
  (interactive)
  (if-let (thing (thing-at-point 'symbol))
      (let ((xref-prompt-for-identifier nil))
        (call-interactively #'xref-find-references))
    (let ((xref-prompt-for-identifier t))
      (call-interactively #'xref-find-references))))

(defun pkg-xref/find-definitions ()
  "Find xref definitions with display action set to 'window, so
that `xref-pop-to-location' uses `pop-to-buffer', which in turn
respects display buffer actions."
  (interactive)
  (lib-util/with-buf-reuse-window
   (cond ((bound-and-true-p lsp-mode)
          (lsp-find-definition :display-action 'window))
         (:default
          (let ((thing (thing-at-point 'symbol)))
            ;; Uses `display-action' as 'window.
            (xref-find-definitions-other-window thing))))))

(lib-util/pkg xref
  :elpaca nil

  :hook ((xref-after-jump-hook
          xref-after-return-hook)
         . recenter)
  :hook ((xref-after-jump-hook
          xref-after-return-hook)
         . xref-pulse-momentarily)

  :init
  (setq xref-search-program 'ripgrep)
  (setq xref-auto-jump-to-first-definition nil)
  (setq xref-auto-jump-to-first-xref nil)

  ;; Prefer global navigation over a history per window. Sometimes it's
  ;; frustrating to jump back to unintended places (e.g. another tab), but I
  ;; still prefer this over `xref-window-local-history'. This is particularly
  ;; important in a configuration like mine where I try as much as possible to
  ;; force commands to reuse visible windows.
  (setq xref-history-storage 'xref-global-history)

  :config
  (advice-add #'xref-pop-to-location :around #'pkg-xref/pop-to-location)
  (advice-add #'xref-pop-marker-stack :around #'pkg-window/display-buffer-reuse-window))

(provide 'pkg-xref)
