;;; -*- lexical-binding: t; -*-

;;; Code:

;;;; Variables

(defgroup lib-pdf nil
  "PDF utilities."
  :group 'my
  :prefix "lib-pdf/")

(defvar-local lib-pdf/file nil
  "PDF file path.")

(defvar-local lib-pdf/metadata nil
  "PDF metadata.")

(define-derived-mode pdf-metadata-mode tabulated-list-mode "pdf-metadata-mode"
  "Major mode to view/edit PDF metadata.")

;;;; Private

(defun lib-pdf/-setup-keymap ()
  (define-key pdf-metadata-mode-map (kbd "<tab>") #'tabulated-list-next-column)
  (define-key pdf-metadata-mode-map (kbd "<backtab>") #'tabulated-list-previous-column)
  (define-key pdf-metadata-mode-map (kbd "C-c a") #'lib-pdf/metadata-add))

(defun lib-pdf/-fetch-metadata (file)
  (let ((cmd (format "exiftool -all:all -json %s" (shell-quote-argument (expand-file-name file)))))
    (thread-first (lib-sys/promise-start-process-shell-command cmd)
                  (promise-then #'json-read-from-string)
                  (promise-then #'seq-first)
                  (promise-catch (lambda (err)
                                   (message "%s" err))))))

(defun lib-pdf/-json->tabulated (json)
  (map-apply (lambda (k v)
               (list (symbol-name k) (format "%s" v)))
             json))

(defun lib-pdf/-write-metadata (file key value)
  (let ((cmd (format "exiftool -overwrite_original -%s=%s %s"
                     key
                     (shell-quote-argument value)
                     (shell-quote-argument (expand-file-name file)))))
    (lib-sys/promise-start-process-shell-command cmd)))

(defun lib-pdf/buffer-name (file)
  (format "*metadata-%s*" (file-name-base file)))

(defun lib-pdf/setup-mode ()
  (lib-pdf/-setup-keymap))

;;; Autoloads

;;;###autoload
(defun lib-pdf/remove-password (file password)
  "Remove password from FILE by using PASSWORD."
  (interactive (list (read-file-name "PDF file: " nil nil t)
                     (read-passwd "Password: ")))
  (thread-first (lib-sys/promise-start-process-shell-command
                 (format "qpdf -password='%s' -decrypt '%s' --replace-input"
                         password (expand-file-name file)))
                (promise-then (lambda (_)
                                (message "Password removed successfully.")))
                (promise-catch (lambda (err)
                                 (message "Failed to remove password: '%s'"
                                          (replace-regexp-in-string "\n$" "" err))))))

;;;###autoload
(defun lib-pdf/metadata (file)
  (interactive "fPDF file: ")
  (thread-first (lib-pdf/-fetch-metadata file)
                (promise-then (lambda (json)
                                (let ((buf (get-buffer-create (lib-pdf/buffer-name file))))
                                  (with-current-buffer buf
                                    (lib-tabulated/display :columns-names '("Property" "Value")
                                                           :rows (lib-pdf/-json->tabulated json)
                                                           :tabulated-mode #'pdf-metadata-mode
                                                           :buffer buf)
                                    (setq lib-pdf/file (expand-file-name file)
                                          lib-pdf/metadata json)))))))

;;;###autoload
(defun lib-pdf/metadata-add (key value)
  (interactive (list (read-string "Metadata key: ")
                     (read-string "Metadata value: ")))
  (thread-first (lib-pdf/-write-metadata lib-pdf/file (intern key) value)
                (promise-then (lambda (_)
                                (message "Metadata '%s' set successfully." key)))
                (promise-catch (lambda (err)
                                 (message "%s" err)))))

(add-hook 'pdf-metadata-mode-hook #'lib-pdf/setup-mode)

(provide 'lib-pdf)
