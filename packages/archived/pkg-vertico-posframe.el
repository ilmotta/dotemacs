;;; -*- lexical-binding: t; -*-
;;; Code:

;; See for mini-frame config, not vertico-posframe
;; https://gist.github.com/rougier/126e358464e12aa28fac5b4f3dd5eb9c

(lib-util/pkg
  (vertico-posframe :ref "7da6d648ff4202a48eb6647ee7dce8d65de48779")
  :defer t
  :init
  (setq vertico-posframe-width nil)
  (setq vertico-posframe-height nil)
  (setq vertico-posframe-min-width nil)
  (setq vertico-posframe-min-height nil)
  (setq vertico-posframe-border-width 4)
  (setq vertico-posframe-truncate-lines t)
  (setq vertico-posframe-parameters '((left-fringe . 8)
                                      (right-fringe . 8)
                                      ;; This has no effect.
                                      (internal-border-width . 50)))

  ;; Use current frame's font as fallback.
  (setq vertico-posframe-font nil))

(provide 'pkg-vertico-posframe)
