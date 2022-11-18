;;; -*- lexical-binding: t; -*-

;;; Commentary:
;; Commands to interact with the Android Debug Bridge (adb).

;;; Code:
(require 'lib-system)

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
    (unless (seq-empty-p devices)
      (completing-read "ADB device: " devices nil t))))

(defun lib-adb/-send-input (device input)
  (call-process-shell-command (format "adb -s '%s' shell input text '%s'"
                                      device
                                      input)))

(defun lib-adb/send-text-to-input ()
  (interactive)
  (if-let ((device (lib-adb/-choose-device)))
      (let ((input (read-string "Input: ")))
        (lib-adb/-send-input device input))
    (message "There are no devices running")))

(provide 'lib-adb)
