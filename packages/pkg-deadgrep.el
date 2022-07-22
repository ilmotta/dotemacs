;;; -*- lexical-binding: t; -*-

(my/package deadgrep
  :straight t
  :defer t
  :init
  ;; Deadgrep will kill the least recently used results buffer if there are more
  ;; than this many. To disable cleanup entirely, set this variable to nil.
  (setq deadgrep-max-buffers nil)

  ;; Truncate lines for performance reasons.
  (setq deadgrep-max-line-length 300)

  (general-def
    :keymaps 'project-prefix-map
    "s S" #'deadgrep))

(provide 'pkg-deadgrep)
