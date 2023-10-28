;;; -*- lexical-binding: t; -*-

(require 'lib-util)

(defun pkg-tempo/tags-completing-insert ()
  (interactive)
  (let ((tag-name (completing-read "Tempo tag: " (map-keys tempo-tags))))
    (insert tag-name)))

(lib-util/pkg tempo
  :elpaca nil
  :config
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
   "Insert a 2x2 HTML table."))

(provide 'pkg-tempo)
