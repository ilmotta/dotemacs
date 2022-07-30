;;; -*- lexical-binding: t; -*-

(require 'promise)
(require 'lib-tabulated)
(require 'lib-util)
(require 'transient)

;;; Variables

(defgroup lib-system nil
  "Manage OS preferences, display settings, etc."
  :group 'my
  :prefix "lib-system/")

(defcustom lib/brightness-debounce-threshold-ms
  1000
  "Debounce threshold in milliseconds."
  :type 'integer)

(defvar lib/raise-brightness-debounced
  (setf (symbol-function 'lib/raise-brightness-debounced)
        (lib-util/debounce-promise lib/brightness-debounce-threshold-ms #'lib/-raise-brightness)))

(defvar lib/lower-brightness-debounced
  (setf (symbol-function 'lib/lower-brightness-debounced)
        (lib-util/debounce-promise lib/brightness-debounce-threshold-ms #'lib/-lower-brightness)))

;;; Private

(defun lib/-round-nearest (n multiple)
  "Round N to the nearest integer multiple of MULTIPLE."
  (let ((n2 (round (* 100 n))))
    (cond ((zerop (% n2 multiple))
           n)
          (t (/ (* multiple (round n2 multiple))
                100.0)))))

(defun lib/-ddcutil-get-brightness ()
  (thread-first
    (lib-system/promise-start-process-shell-command "ddcutil getvcp 10")
    (promise-then (lambda (out)
                    (string-match (rx "current value = " (one-or-more whitespace)
                                      (group (one-or-more digit))
                                      ",")
                                  out)
                    (string-to-number (match-string 1 out))))))

(defun lib/-ddcutil-set-brightness (value)
  (let ((cmd (format "ddcutil setvcp --verify 10 %s" value)))
    (lib-system/promise-start-process-shell-command cmd)))

(defun lib/-ddcutil-message-brightness (value)
  (message "Brightness: %s" (propertize (format "%s" value) 'face 'compilation-info)))

(defun lib/-raise-brightness (amount)
  (thread-first (lib/-ddcutil-get-brightness)
                (promise-then (lambda (brightness)
                                (let ((new-value (min 100 (+ amount brightness))))
                                  (message "%s value" new-value)
                                  (lib/-ddcutil-set-brightness new-value)
                                  new-value)))
                (promise-then (lambda (new-value)
                                (lib/-ddcutil-message-brightness new-value)))
                (promise-catch #'lib-util/message-error)))

(defun lib/-lower-brightness (amount)
  (thread-first (lib/-ddcutil-get-brightness)
                (promise-then (lambda (brightness)
                                (let ((new-value (max 1 (- brightness amount))))
                                  (lib/-ddcutil-set-brightness new-value)
                                  new-value)))
                (promise-then (lambda (new-value)
                                (lib/-ddcutil-message-brightness new-value)))
                (promise-catch #'lib-util/message-error)))

(defun lib/-lower-brightness-20 ()
  (interactive)
  (lib/lower-brightness-debounced 20))

(defun lib/-lower-brightness-10 ()
  (interactive)
  (lib/lower-brightness-debounced 10))

(defun lib/-raise-brightness-20 ()
  (interactive)
  (lib/raise-brightness-debounced 20))

(defun lib/-raise-brightness-10 ()
  (interactive)
  (lib/raise-brightness-debounced 10))

(defun lib/-remove-trailing-newlines (s)
  (replace-regexp-in-string "\n+\\'" "" s))

(defun lib/-shell-command-to-string (cmd)
  (replace-regexp-in-string "\n\\'" ""
                            (shell-command-to-string cmd)))

(defun lib/-start-process-shell-command-async (cmd on-success on-failure)
  "Run CMD in a subprocess. Returns the process instance."
  (with-temp-buffer
    (let* ((name (car (split-string cmd)))
           (buf (get-buffer-create (concat "*libsys-cmd-" (lib-util/uuid) "*")))
           (process (start-process-shell-command name buf cmd)))
      (set-process-sentinel process
                            (lambda (proc _state)
                              (let ((output (with-current-buffer buf
                                              (buffer-string))))
                                (kill-buffer buf)
                                (if (zerop (process-exit-status proc))
                                    (funcall on-success output)
                                  (when on-failure
                                    (funcall on-failure output)))))))))

(defun lib/-get-monitor-name ()
  "Get the connected monitor name."
  (lib/-shell-command-to-string "xrandr | grep ' connected' | cut -f1 -d ' '"))

;;; Public

;;;; Processes

(defun lib/promise-start-process-shell-command (cmd)
  "Run CMD in a subprocess. Returns a promise resolving to the command output."
  (promise-new (lambda (resolve reject)
                 (lib/-start-process-shell-command-async cmd
                                                         (lambda (out)
                                                           (funcall resolve out))
                                                         (lambda (err)
                                                           (funcall reject err))))))

;;;###autoload
(defun lib/pidof (process-name)
  "When called interactively, show a message with the PID of
PROCESS-NAME. When called non-interactively, return a promise
that resolves to the PID string."
  (interactive "sProcess name: ")
  (let ((called-interactively-p (called-interactively-p 'interactive)))
    (thread-first (lib/promise-start-process-shell-command (format "pidof '%s'" process-name))
                  (promise-then #'lib/-remove-trailing-newlines)
                  (promise-then (lambda (pid)
                                  (if called-interactively-p
                                      (message "PID for '%s': %s" process-name pid)
                                    pid))))))

;;;###autoload
(defun lib/process-limits (process-name)
  (interactive "sProcess name: ")
  (thread-first (lib/pidof process-name)
                (promise-catch (lambda (_err)
                                 (message "Could not find PID of '%s'" process-name)))
                (promise-then (lambda (pid)
                                (lib/promise-start-process-shell-command (format "cat /proc/%s/limits" pid))))
                (promise-then #'lib/-remove-trailing-newlines)
                (promise-then (lambda (out)
                                (let* ((data (thread-last (split-string out "\n" 'omit-nulls)
                                                          (seq-map (lambda (line)
                                                                     (split-string line (rx (>= 2 whitespace)))))))
                                       (columns-names (car data))
                                       (data (seq-map (lambda (cols)
                                                        (while (not (equal (length columns-names) (length cols)))
                                                          (setq cols (append cols '(""))))
                                                        cols)
                                                      data)))
                                  (lib-tabulated/display
                                   :columns-names columns-names
                                   :rows (cdr data)))))
                (promise-catch (lambda (err)
                                 (message "%s" err)))
                ))

;;;; Display

;;;###autoload
(defun lib/configure-ultrawide-monitor ()
  (interactive)
  (let* ((width 2560)
         (height 1080)
         (refresh-rate "60.00")
         (device-name "HDMI-1") ; You may need to change it to HDMI1
         (connected-p (equal "connected" (lib/-shell-command-to-string (format "xrandr --current | grep -i %s | cut -f2 -d' '"
                                                                               device-name)))))
    (if connected-p
        (progn
          (let* ((modeline (lib/-shell-command-to-string (format "cvt %s %s %s | cut -f2 -d$'\n'" width height refresh-rate)))
                 (modedata (lib/-shell-command-to-string (format "echo %s | cut -f 3- -d' '" modeline)))
                 (modename (lib/-shell-command-to-string (format "echo %s | cut -f2 -d' '" modedata))))
            (message "Using MODELINE='%s'" modeline)
            (message "Using MODENAME='%s'" modename)
            (message "Using MODEDATA='%s'" modedata)
            (shell-command-to-string (format "xrandr --newmode %s %s" modename modedata))
            (shell-command-to-string (format "xrandr --addmode %s %s" device-name modename))
            (shell-command-to-string (format "xrandr --output %s --mode %s" device-name modename))))
      (message "Error: Could not detect monitor"))))

;;;###autoload
(transient-define-prefix lib/main-t
  "System controllers."
  :transient-non-suffix #'transient--do-quit-one
  [["Brightness"
    ("j" "↓20" lib/-lower-brightness-20 :transient t)
    ("k" "↑20" lib/-raise-brightness-20 :transient t)
    ("J" "↓10" lib/-lower-brightness-10 :transient t)
    ("K" "↑10" lib/-raise-brightness-10 :transient t)]])

;;;; Misc

;;;###autoload
(defun lib/serve-dir (directory)
  "Serve all files in DIRECTORY."
  (interactive "DDirectory: ")
  (start-process-shell-command
   "twistd" nil
   (format "twistd --pidfile='%s' --nodaemon web --listen='tcp:3001' --path='%s'"
           (expand-file-name (format "/tmp/%s.pid" (make-temp-name "twistd-")))
           (file-truename directory))))

;;;; Nix

(defhof lib/--nix-packages
  (lib-util/memoize-ttl
   :ttl-ms (* 1000 3600)
   :f (lambda ()
        (with-temp-buffer
          (let* ((_ (call-process-shell-command "nix-env -f \"<nixpkgs>\" -qaP" nil (buffer-name)))
                 (pkgs (thread-last (split-string (buffer-string) "\n" nil "\n")
                                    (seq-map #'string-trim)
                                    (seq-map (lambda (line)
                                               (replace-regexp-in-string (rx (one-or-more whitespace)) " | " line))))))
            pkgs)))))

;;;###autoload
(defun lib/nix-packages ()
  "List all known Nix packages."
  (interactive)
  (with-temp-buffer
    (let* ((pkgs (lib/--nix-packages))
           (pkg (completing-read "Nix pkgs: " pkgs nil t)))
      pkg)))

;;;; File system

;;;###autoload
(defun lib/resize-tmp (size)
  (interactive "nNew size in GB: ")
  (with-temp-buffer
    (cd "/sudo::")
    (call-process-shell-command (format "mount -o remount,size=%dG /tmp/" size))
    (message "Resized /tmp to %dGB." size)))

(provide 'lib-system)

;; Local Variables:
;; read-symbol-shorthands: (("lib/" . "lib-system/"))
;; End:
