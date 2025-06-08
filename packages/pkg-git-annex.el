;;; -*- lexical-binding: t; -*-

;;; Commentary:
;; There is a package https://github.com/jwiegley/git-annex-el,
;; but it includes features I don't need.
;;
;; For now, this file simply makes annex symlinks more visible by adding a
;; custom SVG badge that displays the text "annex" inside a rounded square.

;;; Code:
(require 'lib-util)

;; Small rounded "annex" badge as an SVG image.
(defvar pkg-git--annex-svg
  (let* ((label "annex")
         (font-size 11)            ; px
         (pad-x 4) (pad-y 2)       ; inner padding around text (inside the box)
         (left-gap 4)              ; transparent space *before* the box
         (right-gap 0)             ; (keep if you also want space after the box)
         ;; crude width estimate (~0.6em per char is fine for a small badge)
         (char-w 0.6)
         (w-box (ceiling (+ (* font-size char-w (length label)) (* 2 pad-x))))
         (h-box (ceiling (+ (* 1.2 font-size) (* 2 pad-y))))
         (rx 3) (ry 3)
         (bg "#888888")            ; gray background
         (fg "#FFFFFF")            ; white text
         (w-total (+ left-gap w-box right-gap))
         (h-total h-box)
         (svg (format
               "<?xml version='1.0'?>
<svg xmlns='http://www.w3.org/2000/svg' width='%d' height='%d' viewBox='0 0 %d %d'>
  <!-- transparent left padding -->
  <g transform='translate(%d 0)'>
    <rect x='0' y='0' width='%d' height='%d' rx='%d' ry='%d' fill='%s'/>
    <text x='%d' y='%d' font-family='sans-serif' font-size='%d'
          fill='%s' text-anchor='middle' dominant-baseline='middle'>%s</text>
  </g>
</svg>"
               w-total h-total w-total h-total
               left-gap
               w-box h-box rx ry bg
               (/ w-box 2) (/ h-box 2) font-size fg label)))
    ;; Use :height so the SVG renders at 1:1 pixels; keeps the padding.
    (create-image svg 'svg t :ascent 'center :height h-total)))

(with-eval-after-load 'dired
  (font-lock-add-keywords
   'dired-mode
   '(("\\(.+\\)\\( -> .*\\.git/annex/.+\\)"
      (dired-move-to-filename) nil
      (2 (list 'face 'default 'display pkg-git--annex-svg) append)))
   'append))

(provide 'pkg-git-annex)
