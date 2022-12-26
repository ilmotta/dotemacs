;;; -*- lexical-binding: t; -*-

;;; Commentary:
;; A minor mode that indicates which buffer is currently active by dimming the
;; faces in the other buffers. It does this non-destructively, and computes the
;; dimmed faces dynamically. I prefer not to enable it at all times, nonetheless
;; I often enable this mode when sharing my screen because it helps other people
;; track where the cursor is.

;;; Code:

(my/package
  (dimmer :ref "a5b697580e5aed6168b571ae3d925753428284f8")
  :defer t

  :init
  (setq dimmer-adjustment-mode :foreground
        dimmer-fraction 0.3)

  :config
  ;; Ensure magit transient buffers are not dimmed.
  (dimmer-configure-magit))

(provide 'pkg-dimmer)
