;;; -*- lexical-binding: t; -*-
(require 'use-package)

(defcustom pkg-mini-frame/fallback-frame-parameters
  `((left . 0)
    (top . 0)
    (width . 1.0)
    (height . 1)
    (left-fringe . 0)
    (right-fringe . 0)
    (horizontal-scroll-bars . nil)
    (vertical-scroll-bars . nil)
    (fullscreen . nil)
    (child-frame-border-width . 8)
    (internal-border-width . 0))
  "Fallback child frame parameters when
`pkg-mini-frame/show-parameters' fails."
  :type 'list)

(defun pkg-mini-frame/bg-color-fn (&optional frame)
  (cond ((equal my/theme 'doom-one) "#1f242b")
        ((equal my/theme 'doom-one-light) "#f0f0f0")
        (:default "#f0f0f0")))

(defun pkg-mini-frame/frame-border-color ()
  (cond ((equal my/theme 'doom-one) "#1f242b")
        ((equal my/theme 'doom-one-light) "#edb3ad")
        (:default "#dddddd")))

(defun pkg-mini-frame/show-parameters ()
  (condition-case err
      (progn
        (require 'posframe)
        (let* ((center-p nil)
               (info (posframe-poshandler-argbuilder))
               (posn (posframe-poshandler-frame-top-left-corner info))
               (left (if center-p 0.5 (car posn)))
               ;; (top (cdr posn))
               (top 0)
               (width (if center-p
                          (let ((parent-frame-width (map-elt info :parent-frame-width)))
                            (cond ((> parent-frame-width 1500) 0.70)
                                  ((> parent-frame-width 1200) 0.90)
                                  (:default 1.0)))
                        1.0)))

          ;; Uncomment to dynamically set the child frame border color.
          ;;
          ;; (when mini-frame-frame
          ;;   (let ((border (pkg-mini-frame/frame-border-color)))
          ;;     (set-face-background 'child-frame-border border mini-frame-frame)
          ;;     (set-face-background 'internal-border border mini-frame-frame)))

          (thread-first pkg-mini-frame/fallback-frame-parameters
                        (map-insert 'left left)
                        (map-insert 'top top)
                        (map-insert 'width width)
                        (map-insert 'font (map-elt (frame-parameters) 'font)))))
    (error
     (message "Failed to build frame parameters in pkg-mini-frame/show-parameters. Error: %s"
              (error-message-string err))
     pkg-mini-frame/fallback-frame-parameters)))

(my/package mini-frame
  :straight (:host github :repo "muffinmad/emacs-mini-frame")
  :defer t

  ;; Disabled on 2022-05-26 because 1) it's incompatible with Vertico; 2)
  ;; Sometimes results are not displayed at all and I couldn't find an issue on
  ;; GitHub.
  :disabled t

  :hook (after-init-hook . mini-frame-mode)

  :init
  ;; Setting to non-nil will cause width/height flickers.
  (setq resize-mini-frames nil)

  ;; If non-nil, mini-frame will be created on first use, instead of on mode activation.
  (setq mini-frame-create-lazy t)

  ;; Unfortunately, the symbol grow-only does not work at all.
  (setq mini-frame-resize t)

  (setq mini-frame-resize-min-height nil)
  (setq mini-frame-resize-max-height nil)
  (setq mini-frame-show-parameters #'pkg-mini-frame/show-parameters)

  (setq mini-frame-color-shift-step 10)
  ;; Uncomment to explicitly handle colors.
  ;; (setq mini-frame-background-color-function #'pkg-mini-frame/bg-color-fn)
  ;; (setq mini-frame-internal-border-color nil)

  (setq mini-frame-ignore-commands
        '(ctrlf-forward-default
          eval-expression
          edebug-eval-expression
          debugger-eval-expression))

  (setq mini-frame-ignore-functions
        '(y-or-n-p
          yes-or-no-p)))

(provide 'pkg-mini-frame)
