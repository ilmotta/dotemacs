;;; -*- lexical-binding: t; -*-

;;; Commentary:
;; Easily define and subsequently act on multiple zones of buffer text - in
;; effect, multiple regions. The package has support for narrowing and
;; unnarrowing indefinitely.

;;; Code:

(defun pkg-zones/monkey-org-narrow-to-subtree (&optional element)
  "Narrow buffer to the current subtree.

Also adds the subtree region using `zz-add-zone'.

Use the command `\\[widen]' to see the whole buffer again."
  (interactive)
  (if (org-element--cache-active-p)
      (let* ((heading (org-element-lineage
                       (or element (org-element-at-point))
                       '(headline) t))
             (end (org-element-property :end heading)))
        (if (and heading end)
            (let ((beg (org-element-property :begin heading))
                  (end (if (= end (point-max))
                           end
                         (1- end))))
              (zz-add-zone beg end)
              (narrow-to-region beg end))
          (signal 'outline-before-first-heading nil)))
    (save-excursion
      (save-match-data
        (org-with-limited-levels
         (let ((beg (progn (org-back-to-heading t) (point)))
               (end (progn (org-end-of-subtree t t)
                           (when (and (org-at-heading-p) (not (eobp))) (backward-char 1))
                           (point))))
           (zz-add-zone beg end)
           (narrow-to-region beg end)))))))

(defun pkg-zones/monkey-org-narrow-to-element ()
  "Narrow buffer to current element.

Also adds the subtree region using `zz-add-zone'.

Use the command `\\[widen]' to see the whole buffer again."
  (interactive)
  (let ((elem (org-element-at-point)))
    (cond
     ((eq (car elem) 'headline)
      (let ((beg (org-element-property :begin elem))
            (end (org-element-property :end elem)))
        (zz-add-zone beg end)
        (narrow-to-region beg end)))
     ((memq (car elem) org-element-greater-elements)
      (let ((beg (org-element-property :contents-begin elem))
            (end (org-element-property :contents-end elem)))
        (zz-add-zone beg end)
        (narrow-to-region beg end)))
     (t
      (let ((beg (org-element-property :begin elem))
            (end (org-element-property :end elem)))
        (zz-add-zone beg end)
        (narrow-to-region beg end))))))

(my/package
  (zones :ref "932aa3cd9e1827c0ebe68a4af7899704c4dba495"
         :host github
         :repo "emacs-straight/zones")
  :defer t

  :init
  (general-def
    :keymaps 'narrow-map
    "x" '(zz-narrow-repeat :properties (:repeat t)))

  :config
  ;; Unfortunately, we need to monkey patch the org functions. The simplest
  ;; solution would be to use a lexical local binding, setting
  ;; `zz-add-zone-anyway-p-' to t, but then `zones' behaves erratically.
  (advice-add #'org-narrow-to-subtree :override #'pkg-zones/monkey-org-narrow-to-subtree)
  (advice-add #'org-narrow-to-element :override #'pkg-zones/monkey-org-narrow-to-element))

(provide 'pkg-zones)
