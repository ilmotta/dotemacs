;;; -*- lexical-binding: t; -*-

;;; Commentary:
;; Commands to interact with the Android Debug Bridge (adb).

;;; Code:

(defvar lib-adb--history-devices nil)
(defvar lib-adb--history-input nil)
(defvar lib-adb--history-paths nil)

(defun lib-adb/-devices ()
  (let* ((output (lib-system/-shell-command-to-string "adb devices"))
         (lines (thread-first output
                              lib-system/-remove-trailing-newlines
                              (split-string "\n" t)
                              cdr)))
    (thread-last lines
                 (seq-map (lambda (line)
                            (split-string line
                                          (rx (one-or-more whitespace))
                                          t)))
                 (seq-remove (lambda (device)
                               (equal "offline" (cdr device))))
                 (seq-map #'car))))

(defun lib-adb/-choose-device ()
  (let ((devices (lib-adb/-devices)))
    (cond ((> (length devices) 1)
           (completing-read "ADB device: " devices nil t nil lib-adb--history-devices))
          ((eq (length devices) 1)
           (seq-first devices)))))

(defun lib-adb/-send-input (device input)
  (call-process-shell-command
   (format "adb -s '%s' shell input text '%s'" device input)))

(defun lib-adb/-pull (device src dst)
  (call-process-shell-command
   (format "adb -s '%s' pull '%s' '%s'" device src dst)))

(defun lib-adb/send-text-to-input ()
  (interactive)
  (if-let ((device (lib-adb/-choose-device)))
      (let ((input (read-string "Input: " "" 'lib-adb--history-input)))
        (lib-adb/-send-input device input))
    (message "There are no devices running")))

(defun lib-adb/restart-as-root ()
  (interactive)
  (eq 0 (call-process "adb" nil nil nil "root")))

(defun lib-adb/file-p (filename)
  "Returns t when FILENAME exists and it's a file."
  (eq 0 (call-process "adb" nil nil nil "shell" "test" "-f" filename)))

(defun lib-adb/exists-p (path)
  "Returns t when PATH exists."
  (eq 0 (call-process "adb" nil nil nil "shell" "test" "-e" path)))

(defun lib-adb/pull ()
  "A safer wrapper around the command adb pull.
If the destination file exists, the user is asked if they want to overwrite it or not."
  (interactive)
  (cl-assert (lib-adb/restart-as-root) nil "Could not restart adb daemon as root")
  (if-let ((device (lib-adb/-choose-device)))
      (let ((src nil)
            (dst nil))
        (setq src (read-string "Source (emulator): " "" 'lib-adb--history-paths))
        (cl-assert (lib-adb/exists-p src) nil "Source path does not exist in device '%s'" device)

        (setq dst (expand-file-name (read-file-name "Destination: " nil default-directory nil)))

        (cond
         ;; Both SRC and DST are files.
         ((and (lib-adb/file-p src)
               (f-file-p dst)
               (not (yes-or-no-p (format "Overwrite file '%s'?" (f-filename dst)))))
          (user-error "Interrupted by user"))

         ;; SRC is a file and DST is a directory, so check that there's no file
         ;; in dst that could be replaced and ask the user.
         ((and (lib-adb/file-p src)
               (f-dir-p dst))
          (let ((new-file (file-name-concat dst (f-filename src))))
            (when (and (f-exists-p new-file)
                       (not (yes-or-no-p (format "Overwrite file '%s'?" (f-filename new-file)))))
              (user-error "Interrupted by user"))))

         ;; SRC is directory and DST is a file.
         ((and (not (lib-adb/file-p src))
               (f-file-p dst))
          (user-error "Invalid paths. Can't pull directory into file."))

         ;; SRC is a directory and DST is a existing directory, it's fine, adb
         ;; will write the directory inside SRC.
         ((and (not (lib-adb/file-p src))
               (f-dir-p dst))
          (lib-adb/-pull device src dst))

         (t (user-error "Invalid paths"))))
    (message "There are no devices running")))

(provide 'lib-adb)
