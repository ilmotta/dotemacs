;;; -*- lexical-binding: t; -*-

;;; Commentary:
;; An Emacs port of quickrun.vim that can run/compile buffers in various
;; languages.

;;; Code:

(my/package quickrun
  :straight t
  :defer t
  :commands (quickrun)
  :init
  (general-def
    :keymaps 'js-mode-map
    :states 'normal
    :prefix "m"
    ;; e[val] q[uickrun]
    "e q" #'quickrun)

  ;; Do not move focus to output buffer.
  (setq quickrun-focus-p nil)

  ;; Avoid infinite loops.
  (setq quickrun-timeout-seconds 5))

(provide 'pkg-quickrun)
