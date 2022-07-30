;;; -*- lexical-binding: t; -*-
;; Copyright (C) 2022 Icaro Motta

;; Author: Icaro Motta <icaro.ldm@gmail.com>
;; URL: http://github.com/ilmotta/dotfiles
;; Version: 1.0.0

;;; Commentary:
;;
;; Alternating between a list of favorite themes and persisting that choice
;; should be straightforward, for instance to choose a light theme during the
;; morning. Emacs doesn't have a concept of /current theme/, so it doesn't
;; ship with any solution to persist and reload the user's choice. The solution
;; below does the simplest possible thing, which is to store the preference in a
;; custom variable which is persisted at ~/.emacs.d/.local/cache/custom.el.
;;
;; Tmux colours were taken from https://github.com/mattdavis90/base16-tmux and
;;
;; Kitty themes: https://github.com/kovidgoyal/kitty-themes/tree/master/themes

;;; Code:
(defun pkg-theme/dark-p ()
  (equal 'dark (frame-parameter nil 'background-mode)))

(defun pkg-theme/write (theme)
  (with-temp-file "~/.theme"
    (erase-buffer)
    (insert (format "SYSTEM_THEME=%s\n" theme))
    (insert (format "THEME_BG_COLOR=%s\n" (face-attribute 'default :background)))))

(defun pkg-theme/update-kitty (theme)
  "Switch Kitty THEME and update all running instances."
  (make-directory "~/.config/kitty" 'parents)
  (let ((kitty-theme (cond ((equal theme 'doom-one) '("Doom_One" . "Doom One"))
                           ((equal theme 'doom-one-light) '("Doom_One_Light" . "Doom One Light"))
                           (:default '("Doom_One" . "Doom One")))))
    (with-temp-file "~/.config/kitty/current-theme.conf"
      (insert "include themes/" (car kitty-theme) ".conf"))))

(defun pkg-theme/update-tmux (theme)
  "Globally change all Tmux windows based on symbol THEME."
  (let* ((script (expand-file-name "~/data/repos/dotfiles/tmux/theme.sh"))
         (cmd (format "%s '%s'" script theme)))
    (start-process-shell-command "tmux" nil cmd)))

(defun pkg-theme/update-system-macos (theme-mode)
  (let ((dark-enabled (if (pkg-theme/dark-p) "true" "false")))
    (start-process-shell-command "osascript" nil
                                 (format "osascript -l JavaScript -e \"Application('System Events').appearancePreferences.darkMode = %s\"" dark-enabled))))

(defun pkg-theme/update-system-gtk (theme-mode)
  (let ((theme (if (pkg-theme/dark-p) my/gtk-dark-theme my/gtk-light-theme)))
    (start-process-shell-command "gsettings" nil
                                 (format "gsettings set org.gnome.desktop.interface gtk-theme %s" theme))))

(defun pkg-theme/update-system-kde (theme-mode)
  ;; Disabled because this has a side-effect of resetting other
  ;; configurations, like the KDE task switcher mode.
  ;;
  ;; (let ((theme (if (pkg-theme/dark-p) my/kde-dark-theme my/kde-light-theme)))
  ;;   (start-process-shell-command "lookandfeeltool" nil
  ;;                                (format "lookandfeeltool --apply %s" theme)))
  )

(defun pkg-theme/gtk? ()
  (equal "GNOME" (getenv "XDG_CURRENT_DESKTOP")))

(defun pkg-theme/kde? ()
  (equal "KDE" (getenv "XDG_CURRENT_DESKTOP")))

(defun pkg-theme/update-system (theme-mode)
  "Change system theme according to THEME-MODE."
  (cond ((pkg-theme/gtk?)
         (pkg-theme/update-system-gtk theme-mode))
        ((pkg-theme/kde?)
         (pkg-theme/update-system-kde theme-mode))
        (my/mac?
         (pkg-theme/update-system-macos theme-mode))))

(defun pkg-theme/merge-face-attributes (face attributes)
  "Make a face spec suitable for `custom-theme-set-faces'.

ATTRIBUTES are appended to the face spec in order to replace
attributes currently used by FACE."
  (let ((face-attributes (face-all-attributes face (selected-frame))))
    `(,face ((t ,@(seq-mapcat (lambda (e) (list (car e) (cdr e))) face-attributes)
              ,@attributes)))))

(defun pkg-theme/customize-common (theme)
  "Update faces with common customizations.

This function should be used to set face attributes that don't
change between dark/light themes and it should be called 'after'a
theme is loaded in order to correctly update all faces."
  (custom-theme-set-faces
   theme
   (pkg-theme/merge-face-attributes 'variable-pitch `(:family ,my/face-variable-pitch-family))
   (pkg-theme/merge-face-attributes 'fixed-pitch `(:family ,my/face-fixed-pitch-family))
   (pkg-theme/merge-face-attributes 'fixed-pitch-serif `(:family ,my/face-fixed-pitch-family))

   (when (pkg-theme/dark-p)
     (pkg-theme/merge-face-attributes 'cursor `(:background "coral1")))

   (cond ((equal my/theme 'doom-one-light)
          (pkg-theme/merge-face-attributes 'shadow `(:foreground "gray65")))))

  (with-eval-after-load 'ctrlf
    (custom-theme-set-faces
     theme
     (pkg-theme/merge-face-attributes 'ctrlf-highlight-active '(:inherit dired-marked))))

  (with-eval-after-load 'org-modern
    (custom-theme-set-faces
     theme
     ;; The weight must be set to bold, otherwise the checkbox under the cursor
     ;; becomes smaller.
     (pkg-theme/merge-face-attributes 'org-modern-symbol '(:weight bold))))

  (with-eval-after-load 'org
    (custom-theme-set-faces
     theme
     (pkg-theme/merge-face-attributes 'org-block '(:inherit fixed-pitch))
     (pkg-theme/merge-face-attributes 'org-table '(:inherit fixed-pitch))
     (pkg-theme/merge-face-attributes 'org-formula '(:inherit fixed-pitch))
     (pkg-theme/merge-face-attributes 'org-verbatim '(:inherit fixed-pitch))
     (pkg-theme/merge-face-attributes 'org-special-keyword '(:inherit fixed-pitch))
     (pkg-theme/merge-face-attributes 'org-meta-line '(:inherit fixed-pitch))
     (pkg-theme/merge-face-attributes 'org-todo '(:inherit fixed-pitch))
     (pkg-theme/merge-face-attributes 'org-document-title '(:height 1.6))))

  (with-eval-after-load 'notmuch
    (custom-theme-set-faces
     theme
     (pkg-theme/merge-face-attributes 'notmuch-search-unread-face '(:weight bold))))

  (with-eval-after-load 'anzu
    (custom-theme-set-faces
     theme
     (pkg-theme/merge-face-attributes 'anzu-replace-highlight '(:inherit (query-replace bold) :strike-through t))
     (pkg-theme/merge-face-attributes 'anzu-replace-to '(:strike-through nil)))))

(defun pkg-theme/update-default-frame-bg-color ()
  (let ((default-bg (face-attribute 'default :background)))
    (setf (cdr (assoc 'background-color default-frame-alist))
          (if (equal "unspecified-bg" default-bg)
              my/default-dark-bg
            default-bg))))

(defun pkg-theme/load-advice (original-fn theme &optional no-confirm no-enable)
  (mapc #'disable-theme (remq theme custom-enabled-themes))
  (when (funcall original-fn theme 'no-confirm no-enable)
    (if (pkg-theme/dark-p)
        (pkg-theme/update-system 'dark)
      (pkg-theme/update-system 'light))
    (setq my/theme theme)
    (pkg-theme/write theme)
    (unless my/windows? (pkg-theme/update-kitty theme))
    (pkg-theme/update-tmux theme)
    (pkg-theme/customize-common theme)
    (pkg-theme/update-default-frame-bg-color)
    (when (fboundp 'org-modern--update-label-face)
      (org-modern--update-label-face))))

(defun pkg-theme/load ()
  (condition-case err
      (load-theme my/theme 'no-confirm)
    (error
     (message "Warning: Could not load theme '%s'. Error '%s'. " my/theme err)
     (unless (equal my/theme 'modus-operandi)
       (message "Loading default theme.")
       (advice-remove 'load-theme #'pkg-theme/load-advice)
       (load-theme 'modus-operandi 'no-confirm)))))

;;; Autoloads

;;;###autoload
(defun pkg-theme/cycle-dark-light ()
  "Alternate between dark and light themes."
  (interactive)
  (cond ((pkg-theme/dark-p)
         (load-theme my/favorite-light-theme 'no-confirm))
        (:default
         (load-theme my/favorite-dark-theme 'no-confirm))))

;;;###autoload
(defun pkg-theme/cycle ()
  "Cycle over themes."
  (interactive)
  (let ((current-theme (car my/favorite-themes))
        (next-theme (cadr my/favorite-themes))
        ;; We need to define a new binding because the advised `load-theme'
        ;; function will save a custom variable and this has the side-effect of
        ;; resetting `my/favorite-themes' to what's persisted on custom.el.
        (favorite-themes my/favorite-themes))
    (load-theme next-theme 'no-confirm)
    (customize-save-variable 'my/favorite-themes
                             (append (cdr favorite-themes)
                                     (list (car favorite-themes))))))

;;; Config

(setq modus-themes-mode-line '(accented borderless))
(setq modus-themes-region '(bg-only))
(setq modus-themes-completions 'opinionated)
(setq modus-themes-bold-constructs t)
(setq modus-themes-italic-constructs nil)
(setq modus-themes-paren-match '(bold intense))
(setq modus-themes-syntax '(alt-syntax))
(setq modus-themes-org-blocks 'tinted-background)
(setq modus-themes-scale-headings t)
;; (setq modus-themes-headings '((1 . (overline))
;;                               (t . t)))

(add-hook 'after-init-hook #'pkg-theme/load)

(general-def
  :keymaps '(my/keys-mode-map)
  :states '(normal visual insert emacs)
  :prefix my/leader
  :non-normal-prefix my/non-normal-prefix
  "x t t" #'pkg-theme/cycle-dark-light)

;; Advice after the theme is first loaded because `pkg-theme/load-advice' calls
;; `customize-save-variable' which is pretty slow.
(advice-add 'load-theme :around #'pkg-theme/load-advice)

(provide 'pkg-theme)
