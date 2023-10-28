;;; -*- lexical-binding: t; -*-

(defun pkg-vterm/prompt-p ()
  "Return t if cursor is at prompt, nil otherwise.
Unfortunately, `vterm--at-prompt-p' only returns true if point is
at the same position returned by `vterm--get-prompt-point'."
  (save-excursion
    (or (vterm--at-prompt-p)
        (progn (vterm-beginning-of-line)
               (vterm--at-prompt-p)))))

;;;###autoload
(defun pkg-vterm/copy-input ()
  "Make input the latest kill in the kill ring.
This implementation does not work with multiline inputs (i.e.
backslash)."
  (interactive)
  (save-excursion
    (let ((start (vterm--get-prompt-point))
          (end nil))
      (goto-char start)
      (setq end (vterm--get-end-of-line))
      (kill-new (buffer-substring-no-properties start end)))))

;;;###autoload
(defun pkg-vterm/copy-output ()
  "Make command output the latest kill in the kill ring.
This implementation does not work with multiline inputs (i.e.
backslash)."
  (interactive)
  (save-excursion
    (let ((start nil)
          (end nil))
      (goto-char (vterm--get-prompt-point))
      (goto-char (vterm--get-end-of-line))
      (if (or (equal (line-number-at-pos)
                     (save-excursion
                       (vterm-next-prompt 1)
                       (line-number-at-pos)))
              (equal (save-excursion
                       (forward-line)
                       (line-number-at-pos))
                     (save-excursion
                       (vterm-next-prompt 1)
                       (line-number-at-pos))))
          ;; Moved to the next prompt, which means there's no output.
          (kill-new "")
        (forward-line)
        (setq start (vterm--get-beginning-of-line))
        (goto-char start)
        (vterm-next-prompt 1)
        (forward-line -1)
        (setq end (vterm--get-end-of-line))
        (kill-new (buffer-substring-no-properties start end))))))

;;;###autoload
(defun pkg-vterm/copy-dwim ()
  "Copy input/output depending if the cursor is on a prompt.
If it's on a prompt, copy the input, otherwise copy the output."
  (interactive)
  (if (pkg-vterm/prompt-p)
      (pkg-vterm/copy-input)
    (pkg-vterm/copy-output)))

(lib-util/pkg vterm
  :elpaca (:ref "f14d113ee4618f052879509ec378feb9766b871b")
  :unless my/android?
  :defer t

  :hook (vterm-mode-hook . lib-sys/set-no-process-query-on-exit)

  :init
  (setq vterm-max-scrollback 9999)
  (setq vterm-timer-delay 0.05)
  ;; (setq vterm-buffer-name-string "*vterm: %s*")
  (setq vterm-kill-buffer-on-exit t)
  (setq vterm-always-compile-module nil)
  (setq vterm-use-vterm-prompt-detection-method t)
  (setq vterm-clear-scrollback-when-clearing nil)

  (general-def
    :keymaps 'vterm-mode-map
    :states '(emacs insert)
    "C-<backspace>" #'vterm-send-C-w)

  (general-def
    :keymaps 'vterm-mode-map
    :states '(normal)
    :prefix "m"
    "y y" #'pkg-vterm/copy-dwim
    "y i" #'pkg-vterm/copy-input
    "y o" #'pkg-vterm/copy-output)

  (general-def
    :keymaps 'vterm-mode-map
    :states '(normal)
    [remap evil-beginning-of-line] #'vterm-beginning-of-line
    "C-k" #'vterm-previous-prompt
    "C-j" #'vterm-next-prompt)

  :config
  (add-to-list 'display-buffer-alist
               `(,(rx line-start "*vterm" (zero-or-more not-newline) line-end)
                 (display-buffer-in-side-window)
                 (window-width . 0.5)
                 (side . right)
                 (slot . 0)
                 (body-function . select-window))))

(provide 'pkg-vterm)
