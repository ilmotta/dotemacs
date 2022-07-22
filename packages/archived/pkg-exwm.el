;;; -*- lexical-binding: t; -*-
(require 'use-package)

;; EXWM (Emacs X Window Manager). IMPORTANT: Do not call `exwm-enable'. We want
;; to be able to open Emacs without EXWM if we haven't started the EXWM session
;; at login.
(my/package exwm
  :disabled t
  :straight (:type git :host github :repo "ch11ng/exwm")
  :demand t
  :config
  (defun my/exwm--truncate-text (name)
    (let ((maxlen 80))
      (if (> (length name) maxlen)
          (concat (cl-subseq name 0 (- maxlen 3)) "...")
        name)))

  (defun my/exwm--class-hook ()
    "Make the window class name the buffer name."
    (exwm-workspace-rename-buffer (my/exwm--truncate-text exwm-class-name)))

  (defun my/exwm--title-hook ()
    "Rename buffer when the window title is available."
    (let* ((buffer-name (if exwm-title
                            (concat exwm-class-name " :: " exwm-title)
                          exwm-class-name)))
      (exwm-workspace-rename-buffer (my/exwm--truncate-text buffer-name))))

  (defun my/exwm--mouse-move (speed direction)
    "Move mouse pointer at the given SPEED and DIRECTION."
    (let ((speed-number (cond ((string= speed "slow") 10)
                              ((string= speed "quick") 30)
                              ((string= speed "fast") 60))))
      (cond ((string= direction "left")  (start-process-shell-command "xdotool" nil (format "xdotool mousemove_relative -- -%s 0" speed-number)))
            ((string= direction "up")    (start-process-shell-command "xdotool" nil (format "xdotool mousemove_relative -- 0 -%s" speed-number)))
            ((string= direction "right") (start-process-shell-command "xdotool" nil (format "xdotool mousemove_relative %s 0" speed-number)))
            ((string= direction "down")  (start-process-shell-command "xdotool" nil (format "xdotool mousemove_relative 0 %s" speed-number))))))

  (defun my/exwm--display-backlight-brightness ()
    (let ((current-value (shell-command-to-string "xbacklight -get")))
      (minibuffer-message "Backlight at %s."
                          (car (split-string current-value "\\." t)))))

  (defun my/exwm--display-volume-level ()
    (let* ((output (shell-command-to-string "amixer -D pulse sget Master"))
           (_ (string-match "\\[\\([0-9]+%\\)\\]" output))
           (current-value (match-string 1 output)))
      (minibuffer-message "Volume level at %s." current-value)))

  (defun my/exwm-lock ()
    "Lock screen with i3lock."
    (interactive)
    (start-process-shell-command "i3lock" nil "i3lock --nofork --ignore-empty-password --color=282828"))

  (defun my/exwm-poweroff (response)
    "Poweroff the machine."
    (interactive (list (y-or-n-p "Power-off machine? ")))
    (when response
      (start-process-shell-command "systemctl" nil "systemctl poweroff")))

  (defun my/exwm-reboot (response)
    "Reboot the machine."
    (interactive (list (y-or-n-p "Reboot machine? ")))
    (when response
      (start-process-shell-command "systemctl" nil "systemctl reboot")))

  (defun my/exwm-raise-brightness (n)
    "Raise backlight brightness by N amount (default 5%)."
    (interactive "P")
    (let ((value (if (numberp n) n 5)))
      (start-process-shell-command "xbacklight" nil (format "xbacklight -inc %s" value))
      (my/exwm--display-backlight-brightness)))

  (defun my/exwm-lower-brightness (n)
    "Lower backlight brightness by N amount (default 5%)."
    (interactive "P")
    (let ((value (if (numberp n) n 5)))
      (start-process-shell-command "xbacklight" nil (format "xbacklight -dec %s" value))
      (my/exwm--display-backlight-brightness)))

  (defun my/exwm-raise-volume (n)
    "Raise master volume by N amount (default 5%)."
    (interactive "P")
    (let* ((value (if (numberp n) n 5))
           (command (concat "amixer -D pulse sset Master " (number-to-string value) "%+")))
      (start-process-shell-command "amixer" nil command)
      (my/exwm--display-volume-level)))

  (defun my/exwm-lower-volume (n)
    "Lower master volume by N amount (default 5%)."
    (interactive "P")
    (let* ((value (if (numberp n) n 5))
           (command (concat "amixer -D pulse sset Master " (number-to-string value) "%-")))
      (start-process-shell-command "amixer" nil command)
      (my/exwm--display-volume-level)))

  (defun my/exwm-toggle-mute ()
    "Toggle mute."
    (interactive)
    (start-process-shell-command "amixer" nil "amixer -D pulse sset Master +1 toggle"))

  (defun my/exwm-run-command (command)
    "Run shell COMMAND without blocking EXWM."
    (interactive (list (read-shell-command "$ ")))
    (start-process-shell-command command nil command))

  (defun my/exwm-browser-search (choice query)
    "Open qutebrowser and search Google/YouTube with QUERY.

Pressing RET or / (forward slash) will search with the default
choice (search using Google)."
    (interactive (list (read-char-choice "Search with [G]oogle or [Y]ouTube? [G]"
                                         '(13 ?/ ?g ?y ?G ?Y))
                       (read-string "Query: ")))
    (let ((error-msg "The query can't be blank. Please, search again."))
      (cond
       ;; Search with Google.
       ((or (char-equal 13 choice) (char-equal ?/ choice)
            (char-equal ?g choice) (char-equal ?G choice))
        (if (s-present-p (s-trim query))
            (let* ((url (url-encode-url (format "https://google.com/search?q=%s" (s-trim query))))
                   (command (format "qutebrowser --target=window '%s'" url)))
              (start-process-shell-command command nil command))
          (message error-msg)))

       ;; Search with YouTube.
       ((or (char-equal ?y choice) (char-equal ?Y choice))
        (if (s-present-p (s-trim query))
            (let* ((url (url-encode-url (format "https://youtube.com/search?q=%s" (s-trim query))))
                   (command (format "qutebrowser --target=window '%s'" url)))
              (start-process-shell-command command nil command))
          (message error-msg))))))

  (defun my/exwm--switch-buffer-regex (regex command)
    "Display first buffer that matches REGEX or run shell COMMAND."
    (let ((buffer (->> (buffer-list)
                       (-filter (lambda (buffer)
                                  (let ((name (buffer-name buffer)))
                                    (string-match regex name))))
                       (-first-item))))
      (if buffer
          (if (get-buffer-window buffer 0)
              (pop-to-buffer buffer)
            (pop-to-buffer-same-window buffer))
        (start-process-shell-command command nil command))))

  ;; (defhydra hydra-session (:hint nil :exit t :foreign-keys nil)
  ;;     "
  ;; SESSION MANAGEMENT
  ;;
  ;; "
  ;;     ("e" save-buffers-kill-terminal "exit")
  ;;     ("l" my/exwm-lock "lock")
  ;;     ("r" my/exwm-reboot "reboot")
  ;;     ("p" my/exwm-poweroff "poweroff")
  ;;     ("q" nil "quit"))

  ;; (defhydra hydra-mouse (:hint nil :exit nil)
  ;;     "
  ;;  MOUSE:
  ;;
  ;;  ^Slow^    ^Quick^    ^Fast^      ^Click^
  ;; ^^^^^^^^-------------------------------------
  ;;  _h_: ←    _H_: ←     _M-H_: ←    _f_ left
  ;;  _k_: ↑    _K_: ↑     _M-K_: ↑    _d_ middle
  ;;  _l_: →    _L_: →     _M-L_: →    _s_ right
  ;;  _j_: ↓    _J_: ↓     _M-J_: ↓
  ;;
  ;; "
  ;;     ("h" (lambda () (interactive) (my/exwm--mouse-move "fast" "left")))
  ;;     ("k" (lambda () (interactive) (my/exwm--mouse-move "fast" "up")))
  ;;     ("l" (lambda () (interactive) (my/exwm--mouse-move "fast" "right")))
  ;;     ("j" (lambda () (interactive) (my/exwm--mouse-move "fast" "down")))
  ;;
  ;;     ("H" (lambda () (interactive) (my/exwm--mouse-move "quick" "left")))
  ;;     ("K" (lambda () (interactive) (my/exwm--mouse-move "quick" "up")))
  ;;     ("L" (lambda () (interactive) (my/exwm--mouse-move "quick" "right")))
  ;;     ("J" (lambda () (interactive) (my/exwm--mouse-move "quick" "down")))
  ;;
  ;;     ("M-H" (lambda () (interactive) (my/exwm--mouse-move "slow" "left")))
  ;;     ("M-K" (lambda () (interactive) (my/exwm--mouse-move "slow" "up")))
  ;;     ("M-L" (lambda () (interactive) (my/exwm--mouse-move "slow" "right")))
  ;;     ("M-J" (lambda () (interactive) (my/exwm--mouse-move "slow" "down")))
  ;;
  ;;     ("f" (lambda () (interactive) (start-process-shell-command "xdotool" nil "xdotool click 1")))
  ;;     ("d" (lambda () (interactive) (start-process-shell-command "xdotool" nil "xdotool click 2")))
  ;;     ("s" (lambda () (interactive) (start-process-shell-command "xdotool" nil "xdotool click 3")))
  ;;
  ;;     ("q" nil "quit"))

  ;; In line mode EXWM receives all keys, but this is not always desirable.
  ;; Translating M-w to C-c is a way to copy text just like in Emacs, otherwise
  ;; we'd have to send C-q C-c (C-q is configured to send next key).
  (setq exwm-input-simulation-keys
        '(([?\M-w] . [C-c])
          ([?\C-y] . [C-v])))

  ;; Show buffers on other workspaces and allow switching to buffers on other
  ;; workspaces. This is like the default Emacs behaviour to switch buffers, but
  ;; you may prefer to isolate buffers in workspaces (in that case set both
  ;; variables to nil).
  (setq exwm-workspace-show-all-buffers t
        exwm-layout-show-all-buffers t)

  (setq exwm-workspace-number 1)

  (add-hook 'exwm-update-class-hook #'my/exwm--class-hook)
  (add-hook 'exwm-update-title-hook #'my/exwm--title-hook)
  (add-hook 'exwm-init-hook #'display-battery-mode)
  (add-hook 'exwm-init-hook #'display-time-mode)

  ;; Global keybindings.
  (exwm-input-set-key (kbd "s-b") #'counsel-switch-buffer)
  (exwm-input-set-key (kbd "s-d") #'my/exwm-run-command)
  (exwm-input-set-key (kbd "s-i") #'exwm-input-toggle-keyboard)
  (exwm-input-set-key (kbd "s-r") #'exwm-reset)
  (exwm-input-set-key (kbd "s-w") #'exwm-workspace-switch)

  ;; Hydras.
  (exwm-input-set-key (kbd "s-M") #'hydra-mouse/body)
  (exwm-input-set-key (kbd "s-Q") #'hydra-session/body)

  ;; Use a simple and limited solution to alternate between two buffers.
  (exwm-input-set-key (kbd "<M-tab>") #'evil-switch-to-windows-last-buffer)

  ;; Quick keybindings to switch apps.
  (exwm-input-set-key
   (kbd "s-.")
   (lambda ()
     (interactive)
     (my/exwm--switch-buffer-regex "^gnome-terminal" "gnome-terminal")))

  (exwm-input-set-key
   (kbd "s-,")
   (lambda ()
     (interactive)
     (my/exwm--switch-buffer-regex "^qutebrowser\s::" "qutebrowser")))

  ;; Searches.
  (exwm-input-set-key (kbd "s-/") #'my/exwm-browser-search)

  ;; Audio controls.
  (exwm-input-set-key (kbd "<XF86AudioRaiseVolume>") #'my/exwm-raise-volume)
  (exwm-input-set-key (kbd "<XF86AudioLowerVolume>") #'my/exwm-lower-volume)
  (exwm-input-set-key (kbd "<XF86AudioMute>") #'my/exwm-toggle-mute)

  ;; Backlight controls.
  (exwm-input-set-key (kbd "<s-up>") #'my/exwm-raise-brightness)
  (exwm-input-set-key (kbd "<s-down>") #'my/exwm-lower-brightness)
  (exwm-input-set-key (kbd "<XF86MonBrightnessUp>") #'my/exwm-raise-brightness)
  (exwm-input-set-key (kbd "<XF86MonBrightnessDown>") #'my/exwm-lower-brightness)

  ;; Enable the evil window management prefix in all X windows.
  (add-to-list 'exwm-input-prefix-keys ?\C-w)

  (add-to-list 'exwm-input-prefix-keys ?\C-q)
  (define-key exwm-mode-map [?\C-q] #'exwm-input-send-next-key))

(provide 'pkg-exwm)
