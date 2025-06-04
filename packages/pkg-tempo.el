;;; -*- lexical-binding: t; -*-

;;; Commentary:
;; Tempo is a powerful snippet expansion system. For examples and documentation:
;;
;; - https://www.emacswiki.org/emacs/TempoSnippets
;; - http://www.lysator.liu.se/~davidk/elisp/tempo-examples.html

;;; Code:

(require 'lib-util)

(defvar-local pkg-tempo/tags-emacs-lisp nil)
(defvar-local pkg-tempo/tags-html nil)

(defun pkg-tempo/tags-completing-insert ()
  (interactive)
  (let ((tag-name (completing-read "Tempo tag: " (map-keys tempo-tags))))
    (insert tag-name)))

(defun pkg-tempo/setup-emacs-lisp ()
  (tempo-define-template
   "emacs-lisp-defun"
   '("(defun " p " (" p ")" n>
     "\"" p "\""
     n> r ")")
   "defun"
   "Insert a defun expression."
   'pkg-tempo/tags-emacs-lisp)

  (tempo-use-tag-list 'pkg-tempo/tags-emacs-lisp))

(defun pkg-tempo/setup-html ()
  (tempo-define-template
   "table-versus"
   '(> "<table>
  <thead>
    <tr>
      <th>" (p "A: " version-a) "</th>
      <th>" (p "B: " version-b) "</th>
    </tr>
  </thead>
  <tbody>
    <tr>
      <td><img src=\"\"/></td>
      <td><img src=\"\"/></td>
    </tr>
  </tbody>
</table>")
   "table-versus"
   "Insert a 2x2 HTML table."
   'pkg-tempo/tags-html)

  (tempo-use-tag-list 'pkg-tempo/tags-html))

(lib-util/pkg tempo
  :ensure nil
  :init
  (add-hook 'emacs-lisp-mode-hook #'pkg-tempo/setup-emacs-lisp)
  (add-hook 'markdown-mode-hook #'pkg-tempo/setup-html)
  (add-hook 'html-mode-hook #'pkg-tempo/setup-html)

  ;; Catch-all default regex.
  (setq-default tempo-match-finder
                (rx line-start
                    (group (one-or-more not-newline))
                    point)))

(provide 'pkg-tempo)
