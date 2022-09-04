;;; -*- lexical-binding: t; -*-

;;;; Variables

(defconst pkg-proced/proced-keywords
  `((,(concat "^\s+\\(.*?\\)\s+\\(.*?\\)\s+\\(.*?\\)\s+\\(.*?\\)\s+"
       "\\(.*?\\)\s+\\(.*?\\)\s+\\(.*\\)")
     (1 'pkg-proced/user)
     (2 'pkg-proced/pid)
     (3 'pkg-proced/cpu)
     (4 'pkg-proced/mem)
     (5 'pkg-proced/time-start)
     (6 'pkg-proced/time-duration)
     (7 'pkg-proced/process)))
  "Extra font-lock patterns for the `proced' menu.")

;;;; Faces

(defface pkg-proced/user '((t :inherit shadow))
  "Face for user indicator in `proced'."
  :group 'process)

(defface pkg-proced/pid
  '((((class color) (min-colors 88) (background light))
     :foreground "#5317ac")
    (((class color) (min-colors 88) (background dark))
     :foreground "#b6a0ff"))
  "Face for PID indicator in `proced'."
  :group 'process)

(defface pkg-proced/cpu
  '((((class color) (min-colors 88) (background light))
     :foreground "#8f0075")
    (((class color) (min-colors 88) (background dark))
     :foreground "#f78fe7"))
  "Face for memory indicator in `proced'."
  :group 'process)

(defface pkg-proced/mem
  '((((class color) (min-colors 88) (background light))
     :foreground "#0031a9")
    (((class color) (min-colors 88) (background dark))
     :foreground "#2fafff"))
  "Face for CPU indicator in `proced'."
  :group 'process)

(defface pkg-proced/time-start
  '((((class color) (min-colors 88) (background light))
     :foreground "#30517f")
    (((class color) (min-colors 88) (background dark))
     :foreground "#a0bfdf"))
  "Face for start time indicator in `proced'."
  :group 'process)

(defface pkg-proced/time-duration
  '((((class color) (min-colors 88) (background light))
     :foreground "#00538b")
    (((class color) (min-colors 88) (background dark))
     :foreground "#00cdc8"))
  "Face for time indicator in `proced'."
  :group 'process)

(defface pkg-proced/process nil
  "Face for process indicator in `proced'."
  :group 'process)

;;;; Autoloads

;;;###autoload
(define-minor-mode pkg-proced/extra-keywords
  "Apply extra font-lock rules to diff buffers."
  :init-value nil
  :global t
  (if pkg-proced/extra-keywords
      (progn
        (font-lock-flush (point-min) (point-max))
        (font-lock-add-keywords nil pkg-proced/proced-keywords nil)
        (add-hook 'proced-mode-hook #'pkg-proced/extra-keywords))
    (font-lock-remove-keywords nil pkg-proced/proced-keywords)
    (remove-hook 'proced-mode-hook #'pkg-proced/extra-keywords)
    (font-lock-flush (point-min) (point-max))))

(my/package proced
  :straight (:type built-in)
  :defer t
  :hook (proced-mode-hook . pkg-proced/extra-keywords)
  :init
  (setq-default proced-auto-update-flag t)
  (setq proced-auto-update-interval 2)
  (setq proced-descend t)
  (setq proced-filter 'user))

(provide 'pkg-proced)
