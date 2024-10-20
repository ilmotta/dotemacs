;;; -*- lexical-binding: t; -*-
;;; Code:

;; See for mini-frame config, not vertico-posframe
;; https://gist.github.com/rougier/126e358464e12aa28fac5b4f3dd5eb9c

(lib-util/pkg vertico-posframe
  :elpaca (:host github
           :repo "tumashu/vertico-posframe"
           :ref "5d9604dcc3ccb3fd7d69da58382b920484f890c8")
  :defer t
  :init
  ;; NOTE: This is useful when emacs is used in both in X and terminal, for
  ;; posframe do not work well in terminal, so vertico-buffer-mode will be used
  ;; as fallback at the moment.
  (setq vertico-posframe-fallback-mode #'vertico-buffer-mode)

  (setq vertico-posframe-width nil)
  (setq vertico-posframe-height nil)
  ;; Expand to max available width
  (setq vertico-posframe-min-width nil)
  (setq vertico-posframe-min-height nil)
  (setq vertico-posframe-border-width 0)
  (setq vertico-posframe-truncate-lines t)
  (setq vertico-posframe-vertico-multiform-key nil)
  (setq vertico-posframe-poshandler #'posframe-poshandler-frame-center)
  (setq vertico-posframe-parameters '((left-fringe . 10)
                                      (right-fringe . 10)
                                      (no-special-glyphs . t)

                                      (child-frame-border-width . nil)
                                      (internal-border-width . 5)))

  ;; Use current frame's font as fallback.
  (setq vertico-posframe-font nil))

(provide 'pkg-vertico-posframe)
