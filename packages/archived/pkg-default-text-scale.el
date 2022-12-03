;;; -*- lexical-binding: t; -*-

;;; Commentary:
;; Disabled on 2022-12-03 because Emacs 29.1 has `global-text-scale-adjust'.

;;; Code:

(defun pkg-text-scale/buffer-increase ()
  "Increase the height of the default face in the current buffer by
1 step."
  (interactive)
  (text-scale-increase 1))

(defun pkg-text-scale/buffer-decrease ()
  "Decrease the height of the default face in the current buffer by
1 step."
  (interactive)
  (text-scale-decrease 1))

(defun pkg-text-scale/buffer-reset ()
  "Reset the scale factor of the default face in the current buffer
to zero."
  (interactive)
  (text-scale-set 0))

(transient-define-prefix pkg-transient/zoom-t
  "Control global/buffer zoom."
  :transient-non-suffix #'transient--do-quit-one
  [["Zoom Global"
    ("+" "increase" default-text-scale-increase :transient t)
    ("-" "decrease" default-text-scale-decrease :transient t)
    ("0" "reset" default-text-scale-reset :transient t)]
   ["Zoom Buffer"
    ("k" "increase" pkg-text-scale/buffer-increase :transient t)
    ("j" "decrease" pkg-text-scale/buffer-decrease :transient t)
    ("r" "reset" pkg-text-scale/buffer-reset :transient t)]])

;; Easily adjust the font size in all Emacs frames. The bindings are the same
;; used in web browsers.
(my/package default-text-scale
  :straight t
  :defer t
  :init
  (general-def
    :keymaps 'my/keys-mode-map
    "C-c z" #'pkg-transient/zoom-t))

(provide 'pkg-default-text-scale)
