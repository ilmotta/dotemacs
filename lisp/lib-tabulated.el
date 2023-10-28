;;; -*- lexical-binding: t; -*-

;;; Commentary:
;; A basic abstraction of the `tabulated-list-mode'.

;;; Code:
(require 'cl-macs)
(require 'dash)

(defgroup lib-tabulated nil
  "Utilities to display tabulated data."
  :group 'my
  :prefix "lib-tabulated/")

(defcustom lib-tabulated/*padding* 1
  "Number of characters preceding each Tabulated List mode entry."
  :type 'integer)

(defcustom lib-tabulated/*padding-between* 2
  "Extra whitespace between each cell."
  :type 'integer)

(defcustom lib-tabulated/*max-width* 50
  "Max width for any given column."
  :type 'integer)

(defcustom lib-tabulated/*buffer-name* "*tabulated*"
  "Buffer name for tabulated buffers."
  :type 'string)

(defun lib-tabulated/headers (columns-names rows)
  "Return headers suitable for `tabulated-list-format', where each
column width is calculated from all values under that
column (including the column name itself)."
  (let ((sortable t)
        (widths (-reduce-from (lambda (widths row)
                                (if row ; A row without columns doesn't require width computation.
                                    (-zip-with (lambda (width col)
                                                 (min lib-tabulated/*max-width*
                                                      (max width (length col))))
                                               widths
                                               row)
                                  widths))
                              (seq-map #'length columns-names)
                              rows)))
    (seq-into (-zip-with (lambda (col width)
                           (list col (+ width lib-tabulated/*padding-between*) sortable))
                         columns-names
                         widths)
              #'vector)))

(cl-defun lib-tabulated/display (&key
                                 columns-names
                                 rows
                                 (tabulated-mode #'tabulated-list-mode)
                                 (buffer lib-tabulated/*buffer-name*))
  (with-current-buffer (get-buffer-create buffer)
    (funcall tabulated-mode)
    (hl-line-mode +1)
    (setq tabulated-list-format (lib-tabulated/headers columns-names rows))
    (setq tabulated-list-sort-key (cons (seq-first columns-names) nil))
    (setq tabulated-list-padding lib-tabulated/*padding*)
    (setq tabulated-list-entries
          (seq-map-indexed (lambda (row id)
                             (list id (seq-into row #'vector)))
                           rows))
    (tabulated-list-init-header)
    (tabulated-list-print 'remember-pos)
    (pop-to-buffer (current-buffer))))

(provide 'lib-tabulated)
