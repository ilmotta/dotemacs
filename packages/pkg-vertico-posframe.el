;;; -*- lexical-binding: t; -*-
;;; Code:

;; See for mini-frame config, not vertico-posframe
;; https://gist.github.com/rougier/126e358464e12aa28fac5b4f3dd5eb9c

(defvar pkg-vertico-posframe/-posframe-setting-top-full-width
  '(posframe
    (vertico-posframe-poshandler . posframe-poshandler-frame-top-center)
    (vertico-posframe-min-width . 1000)))

(defvar pkg-vertico-posframe/-posframe-setting-center-fixed-width
  '(posframe
    (vertico-posframe-width . 100)
    (vertico-posframe-poshandler . posframe-poshandler-frame-top-center)))

(lib-util/pkg vertico-posframe
  :elpaca (:host github
           :repo "tumashu/vertico-posframe"
           :ref "c5a8b5f72a582e88a2a696a3bbc2df7af28bd229")
  ;; Even after disabling all packages and all custom code, I couldn't get the
  ;; cursor to be displayed.
  :disabled t
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
  (setq vertico-posframe-poshandler #'posframe-poshandler-frame-top-center)
  (setq vertico-posframe-parameters '((left-fringe . 10)
                                      (right-fringe . 10)
                                      (no-special-glyphs . t)

                                      (child-frame-border-width . nil)
                                      (internal-border-width . 5)))

  ;; Use current frame's font as fallback.
  (setq vertico-posframe-font nil)

  :config
  (with-eval-after-load 'vertico
    (setq vertico-multiform-commands
          `(,@(seq-map (lambda (e)
                         (cons e pkg-vertico-posframe/-posframe-setting-top-full-width))
               '(consult-bookmark
                 consult-line
                 consult-ripgrep
                 lsp-find-references
                 pkg-consult/ripgrep-dwim))

            ,@(seq-map (lambda (e)
                         (cons e pkg-vertico-posframe/-posframe-setting-center-fixed-width))
               '(consult-buffer
                 consult-project-buffer
                 find-file
                 project-find-file
                 pkg-tab-bar/switch-project-as-tab
                 project-other-tab-command
                 project-switch-project))

            (t posframe)))))

(provide 'pkg-vertico-posframe)
