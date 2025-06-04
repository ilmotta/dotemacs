;;; -*- lexical-binding: t; -*-

;;; Code:

;;; Variables

(defgroup lib-sys nil
  "Manage OS preferences, display settings, etc."
  :group 'my
  :prefix "lib-sys/")

(defcustom lib-sys/brightness-debounce-threshold-ms
  10
  "Debounce threshold in milliseconds."
  :type 'integer)

(defvar lib-sys/-brightness 100)
(defvar lib-sys/-timer-brightness nil)

(defun lib-sys/init-brightness ()
  (run-with-timer 1 nil (lambda ()
                          (thread-first
                            (lib-sys/-ddcutil-get-brightness)
                            (promise-then (lambda (brightness)
                                            (setq lib-sys/-brightness brightness)))
                            (promise-catch (lambda (err)
                                             (let ((inhibit-message t))
                                               (message "Failed to get brightness"))))))))

(when (executable-find "ddcutil")
  (lib-sys/init-brightness))

(defvar lib-sys/raise-brightness-debounced
  (setf (symbol-function 'lib-sys/raise-brightness-debounced)
        (lib-util/debounce-promise lib-sys/brightness-debounce-threshold-ms #'lib-sys/-raise-brightness)))

(defvar lib-sys/lower-brightness-debounced
  (setf (symbol-function 'lib-sys/lower-brightness-debounced)
        (lib-util/debounce-promise lib-sys/brightness-debounce-threshold-ms #'lib-sys/-lower-brightness)))

;;; Private

(defun lib-sys/-round-nearest (n multiple)
  "Round N to the nearest integer multiple of MULTIPLE."
  (let ((n2 (round (* 100 n))))
    (cond ((zerop (% n2 multiple))
           n)
          (t (/ (* multiple (round n2 multiple))
                100.0)))))

(defun lib-sys/-ddcutil-get-brightness ()
  (thread-first
    (lib-sys/promise-start-process-shell-command "ddcutil getvcp 10")
    (promise-then (lambda (out)
                    (string-match (rx "current value = " (one-or-more whitespace)
                                      (group (one-or-more digit))
                                      ",")
                                  out)
                    (string-to-number (match-string 1 out))))))

(defun lib-sys/-ddcutil-set-brightness (value)
  (let ((cmd (format "ddcutil setvcp --verify 10 %s" value)))
    (lib-sys/promise-start-process-shell-command cmd)))

(defun lib-sys/-ddcutil-message-brightness (value)
  (message "Brightness: %s" (propertize (format "%s" value) 'face 'compilation-info)))

(defun lib-sys/-raise-brightness (amount)
  (when lib-sys/-timer-brightness
    (cancel-timer lib-sys/-timer-brightness))
  (let ((new-val (min 100 (+ amount lib-sys/-brightness))))
    (setq lib-sys/-brightness new-val)
    (lib-sys/-ddcutil-message-brightness new-val)
    (promise-new
     (lambda (resolve reject)
       (setq lib-sys/-timer-brightness
             (run-with-timer
              1
              nil
              (lambda ()
                (thread-first
                  (lib-sys/-ddcutil-set-brightness new-val)
                  (promise-then resolve)
                  (promise-catch (lambda (err)
                                   (lib-util/message-error err)
                                   (reject err)))))))))))

(defun lib-sys/-lower-brightness (amount)
  (when lib-sys/-timer-brightness
    (cancel-timer lib-sys/-timer-brightness))
  (let ((new-val (max 1 (- lib-sys/-brightness amount))))
    (setq lib-sys/-brightness new-val)
    (lib-sys/-ddcutil-message-brightness new-val)
    (promise-new
     (lambda (resolve reject)
       (setq lib-sys/-timer-brightness
             (run-with-timer
              1
              nil
              (lambda ()
                (thread-first
                  (lib-sys/-ddcutil-set-brightness new-val)
                  (promise-then resolve)
                  (promise-catch (lambda (err)
                                   (lib-util/message-error err)
                                   (reject err)))))))))))

(defun lib-sys/-lower-brightness-5 ()
  (interactive)
  (lib-sys/lower-brightness-debounced 5))

(defun lib-sys/-raise-brightness-5 ()
  (interactive)
  (lib-sys/raise-brightness-debounced 5))

(defun lib-sys/-remove-trailing-newlines (s)
  (replace-regexp-in-string "\n+\\'" "" s))

(defun lib-sys/-shell-command-to-string (cmd)
  (replace-regexp-in-string "\n\\'" ""
                            (shell-command-to-string cmd)))

(defun lib-sys/-start-process-shell-command-async (cmd on-success on-failure)
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

(defun lib-sys/-get-monitor-name ()
  "Get the connected monitor name."
  (lib-sys/-shell-command-to-string "xrandr | grep ' connected' | cut -f1 -d ' '"))

;;; Public

;;;; Processes

(defun lib-sys/set-no-process-query-on-exit ()
  "Don't ask for confirmation to exit the process' buffer."
  (let ((proc (get-buffer-process (current-buffer))))
    (when (processp proc)
      (set-process-query-on-exit-flag proc nil))))

(defun lib-sys/promise-start-process-shell-command (cmd)
  "Run CMD in a subprocess. Returns a promise resolving to the command output."
  (promise-new (lambda (resolve reject)
                 (lib-sys/-start-process-shell-command-async cmd
                                                         (lambda (out)
                                                           (funcall resolve out))
                                                         (lambda (err)
                                                           (funcall reject err))))))

;;;###autoload
(defun lib-sys/pidof (process-name)
  "When called interactively, show a message with the PID of
PROCESS-NAME. When called non-interactively, return a promise
that resolves to the PID string."
  (interactive "sProcess name: ")
  (let ((called-interactively-p (called-interactively-p 'interactive)))
    (thread-first (lib-sys/promise-start-process-shell-command (format "pidof '%s'" process-name))
                  (promise-then #'lib-sys/-remove-trailing-newlines)
                  (promise-then (lambda (pid)
                                  (if called-interactively-p
                                      (message "PID for '%s': %s" process-name pid)
                                    pid))))))

;;;###autoload
(defun lib-sys/process-limits (process-name)
  (interactive "sProcess name: ")
  (thread-first (lib-sys/pidof process-name)
                (promise-catch (lambda (_err)
                                 (message "Could not find PID of '%s'" process-name)))
                (promise-then (lambda (pid)
                                (lib-sys/promise-start-process-shell-command (format "cat /proc/%s/limits" pid))))
                (promise-then #'lib-sys/-remove-trailing-newlines)
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
                                 (message "%s" err)))))

;;;; Display

;;;###autoload
(defun lib-sys/configure-ultrawide-monitor ()
  (interactive)
  (let* ((width 2560)
         (height 1080)
         (refresh-rate "60.00")
         (device-name "HDMI-1") ; You may need to change it to HDMI1
         (connected-p (equal "connected" (lib-sys/-shell-command-to-string (format "xrandr --current | grep -i %s | cut -f2 -d' '"
                                                                                   device-name)))))
    (if connected-p
        (progn
          (let* ((modeline (lib-sys/-shell-command-to-string (format "cvt %s %s %s | cut -f2 -d$'\n'" width height refresh-rate)))
                 (modedata (lib-sys/-shell-command-to-string (format "echo %s | cut -f 3- -d' '" modeline)))
                 (modename (lib-sys/-shell-command-to-string (format "echo %s | cut -f2 -d' '" modedata))))
            (message "Using MODELINE='%s'" modeline)
            (message "Using MODENAME='%s'" modename)
            (message "Using MODEDATA='%s'" modedata)
            (shell-command-to-string (format "xrandr --newmode %s %s" modename modedata))
            (shell-command-to-string (format "xrandr --addmode %s %s" device-name modename))
            (shell-command-to-string (format "xrandr --output %s --mode %s" device-name modename))))
      (message "Error: Could not detect monitor"))))

(with-eval-after-load 'transient
  (transient-define-prefix lib-sys/main-t ()
    "System controllers."
    :transient-non-suffix #'transient--do-quit-one
    [["Brightness"
      ("j" "↓5" lib-sys/-lower-brightness-5 :transient t)
      ("k" "↑5" lib-sys/-raise-brightness-5 :transient t)]]))

;;;; Misc

;;;###autoload
(defun lib-sys/serve-dir (directory)
  "Serve all files in DIRECTORY."
  (interactive "DDirectory: ")
  (start-process-shell-command
   "twistd" nil
   (format "twistd --pidfile='%s' --nodaemon web --listen='tcp:3001' --path='%s'"
           (expand-file-name (format "/tmp/%s.pid" (make-temp-name "twistd-")))
           (file-truename directory))))

;;;; Nix

(defhof
 lib-sys/--nix-packages
 (lib-util/memoize-ttl
  :ttl-ms (* 8 1000 3600)
  :f (lambda ()
       (with-temp-buffer
         (let* ((_ (call-process-shell-command "nix-env -f \"<nixpkgs>\" -qaP" nil (buffer-name)))
                (pkgs (thread-last (split-string (buffer-string) "\n" nil "\n")
                                   (seq-map #'string-trim)
                                   (seq-map (lambda (line)
                                              (replace-regexp-in-string (rx (one-or-more whitespace)) " | " line))))))
           pkgs)))))

;;;###autoload
(defun lib-sys/nix-packages ()
  "List all known Nix packages."
  (interactive)
  (with-temp-buffer
    (let* ((pkgs (lib-sys/--nix-packages))
           (pkg (completing-read "Nix pkgs: " pkgs nil t)))
      pkg)))

;;;; File system

;;;###autoload
(defun lib-sys/resize-tmp (size)
  (interactive "nNew size in GB: ")
  (with-temp-buffer
    (cd "/sudo::")
    (call-process-shell-command (format "mount -o remount,size=%dG /tmp/" size))
    (message "Resized /tmp to %dGB." size)))

(provide 'lib-sys)
