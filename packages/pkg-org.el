;;; -*- lexical-binding: t; -*-

(require 'lib-util)

;;; Utils

(defun pkg-org/export-output-file-name (orig-fun extension &optional subtreep pub-dir)
  "Export org files to ./export.

This is a simple solution to avoid littering directories with
PDFs, HTML and Tex files."
  (let ((pub-dir (or pub-dir "./export")))
    (unless (file-directory-p pub-dir)
      (make-directory pub-dir))
    (apply orig-fun extension subtreep pub-dir nil)))

(defun pkg-org/recenter (&rest _args)
  (recenter))

(defun pkg-org/note-id ()
  (format-time-string my/note-id-time-string (current-time) "UTC0"))

;;; Custom link types

(defun pkg-org/link-mpv-complete ()
  (let ((file (read-file-name "File: "))
        (start (read-string "Start time: " "00:00:00")))
    (format "mpv:%s:start=%s" file start)))

(defun pkg-org/link-mpv-follow (path)
  (let* ((props-index (string-match ":" path))
         (props (when props-index
                  (seq-map (lambda (prop)
                             (let ((res (split-string prop "=")))
                               `(,(car res) . ,(cadr res))))
                           (split-string (substring path (1+ props-index))
                                         "&"))))
         (file (if props-index
                   (substring path 0 props-index)
                 path))
         (cmd (format "mpv %s %s"
                      (string-join (seq-map (lambda (prop)
                                              (format "--%s=%s" (car prop) (cdr prop)))
                                            props)
                                   " ")
                      (shell-quote-argument (file-truename file)))))
    (start-process-shell-command "mpv" nil cmd)))

;;; Autoloads

;;;###autoload
(defun pkg-org/copy-symbol-at-point-as-link ()
  "Copy symbol at point as an org file link."
  (interactive)
  (kill-new (format "[[file:%s::%d][%s]]"
                    (buffer-file-name)
                    (line-number-at-pos)
                    (or (thing-at-point 'symbol)
                        (file-name-nondirectory (directory-file-name (buffer-file-name)))))))

;;;###autoload
(defun pkg-org/link-unlink ()
  "Unlink the text at point."
  (interactive)
  (unless (org-in-regexp org-link-bracket-re 1)
    (user-error "No link at point"))
  (save-excursion
    (let ((label (if (match-end 2)
                     (match-string-no-properties 2)
                   (org-link-unescape (match-string-no-properties 1)))))
      (delete-region (match-beginning 0) (match-end 0))
      (insert label))))

;;;###autoload
(defun pkg-org/link-delete ()
  "Delete org link at point."
  (interactive)
  (unless (org-in-regexp org-link-bracket-re 1)
    (user-error "No link at point"))
  (save-excursion
    (let ((label (if (match-end 2)
                     (match-string-no-properties 2)
                   (org-link-unescape (match-string-no-properties 1)))))
      (kill-new (string-replace "\n" " " (buffer-substring-no-properties (match-beginning 0) (match-end 0))))
      (delete-region (match-beginning 0) (match-end 0)))))

;;;###autoload
(defun pkg-org/link-copy ()
  "Copy org link at point."
  (interactive)
  (unless (org-in-regexp org-link-bracket-re 1)
    (user-error "No link at point"))
  (let* ((element (org-element-context))
         (type (car element))
         (props (cadr element)))
    (when (equal type 'link)
      (let ((raw-link (map-elt props :raw-link))
            (beg (map-elt props :contents-begin))
            (end (map-elt props :contents-end)))
        (kill-new (concat "[[" raw-link "]"
                          (if (and beg end)
                              (format "[%s]]" (string-replace "\n" " " (buffer-substring-no-properties beg end)))
                            "]")))))))

;;;###autoload
(defun pkg-org/link-copy-url ()
  "Copy link URL at point."
  (interactive)
  (unless (org-in-regexp org-link-bracket-re 1)
    (user-error "No link at point"))
  (let* ((element (org-element-context))
         (type (car element))
         (props (cadr element)))
    (when (equal type 'link)
      (kill-new (map-elt props :raw-link)))))

;;;###autoload
(defun pkg-org/rename-file-at-point ()
  "Rename file link at point."
  (interactive)
  (if-let (path (org-element-property :path (org-element-context)))
      (let* ((ext (file-name-extension path))
             (dir-path (file-name-directory path))
             (current-name (file-name-nondirectory path))
             (new-path (read-file-name "Rename file to: " dir-path current-name)))
        ;; Depending on how the user autocompletes the file name, `new-path' may
        ;; only contain the new file name (without directory information). In
        ;; that case, we concatenate the current link directory.
        (unless (file-name-absolute-p new-path)
          (setq new-path (concat dir-path new-path)))

        ;; Make sure the new link uses relative paths if the current link is
        ;; relative.
        (unless (file-name-absolute-p path)
          (setq new-path (file-relative-name new-path default-directory)))

        (when-let ((dir (file-name-directory new-path)))
          (cl-assert (file-exists-p dir) nil (format "Directory does not exist '%s'" new-path)))

        (rename-file path new-path)

        ;; Rename all occurrences of `path' to `new-path'.
        (save-excursion
          (goto-char (point-min))
          (while (re-search-forward path nil t)
            (replace-match new-path))))
    (message "Error: Not a link")))

;;;###autoload
(defun pkg-org/add-ids-to-headlines ()
  "Add ID properties to all headlines in the current file which do
not already have one."
  (interactive)
  (org-map-entries 'org-id-get-create))

;;;###autoload
(defun pkg-org/add-ids (level-matcher)
  "Add ID properties to all headlines matching the LEVEL-MATCHER,
unless the ID property already exists."
  (interactive (list (completing-read "Add Org IDs to headlines: "
                                      '("ALL"
                                        "<= 2"
                                        "<= 3"
                                        "= 1"
                                        "= 2"
                                        "= 3"))))
  (org-map-entries (lambda ()
                     (let* ((headline (org-element-headline-parser))
                            (level (org-element-property :level headline)))
                       (cond ((equal level-matcher "ALL")
                              (org-id-get-create))
                             ((and (equal level-matcher "<= 2")
                                   (or (equal level 1) (equal level 2)))
                              (org-id-get-create))
                             ((and (equal level-matcher "<= 3")
                                   (or (equal level 1) (equal level 2) (equal level 3)))
                              (org-id-get-create))
                             ((and (equal level-matcher "= 1")
                                   (equal level 1))
                              (org-id-get-create))
                             ((and (equal level-matcher "= 2")
                                   (equal level 2))
                              (org-id-get-create))
                             ((and (equal level-matcher "= 3")
                                   (equal level 3))
                              (org-id-get-create)))))))

;;;###autoload
(defun pkg-org/open-at-point ()
  (interactive)
  (lib-util/with-buf-reuse-window
   (call-interactively #'org-open-at-point)))

;;; Package

(lib-util/pkg org
  ;; Release 9.6.7 https://git.savannah.gnu.org/cgit/emacs/org-mode.git/commit/?h=release_9.6.7&id=ca873f7fe47546bca19821f1578a6ab95bf5351c
  ;;
  ;; Can't go to latest because org-roam will fail processing some files
  ;; https://github.com/org-roam/org-roam/issues/2381
  :elpaca (:ref "ca873f7fe47546bca19821f1578a6ab95bf5351c")
  :defer t

  :init
;;;; Keybindings
  (general-def
    :keymaps 'org-mode-map
    ;; This is too valuable to be taken by org buffers. Original
    ;; `org-cycle-agenda-files'.
    "C-," nil
    [remap org-open-at-point] '(pkg-org/open-at-point :properties (:jump t)))

  (general-def
    :keymaps 'org-src-mode-map
    ;; "C-c C-c" (instead of "C-c '") is how we usually commit changes in edit
    ;; buffers.
    "C-c C-c" #'org-edit-src-exit)

  (general-def
    :keymaps 'my/keys-mode-map
    :states '(normal )
    :prefix my/leader
    :non-normal-prefix my/non-normal-prefix
    "o a" #'org-agenda)

  (general-def
    :keymaps 'org-agenda-mode-map
    :states '(motion emacs)
    ;; Make it consistent with Dired.
    "U" #'org-agenda-bulk-unmark)

  (general-def
    :keymaps 'my/keys-mode-map
    "C-c c" #'org-capture)

  (my/general-mode-def
    :keymaps 'org-mode-map
    ;; Link
    "l d" #'pkg-org/link-delete
    "l u" #'pkg-org/link-unlink
    "l y" #'pkg-org/link-copy
    "l Y" #'pkg-org/link-copy-url

    ;; Tree/subtree
    "s a" #'org-toggle-archive-tag
    "s A" #'org-archive-subtree
    "s b" #'org-tree-to-indirect-buffer
    "s c" #'org-clone-subtree-with-time-shift
    "s d" #'org-cut-subtree
    "s h" #'org-promote-subtree
    "s j" #'org-move-subtree-down
    "s k" #'org-move-subtree-up
    "s l" #'org-demote-subtree
    "s n" #'org-toggle-narrow-to-subtree
    "s r" #'org-refile
    "s s" #'org-sparse-tree
    "s S" #'org-sort

    ;; Toggles
    "t c" #'org-toggle-comment
    "t e" #'org-toggle-pretty-entities
    "t h" #'org-toggle-heading
    "t i" #'org-toggle-inline-images
    "t l" #'org-toggle-link-display
    "t t" #'org-toggle-item
    "t x" #'org-toggle-checkbox

    ;; Date/deadline
    "d d" #'org-deadline
    "d s" #'org-schedule
    "d t" #'org-time-stamp
    "d T" #'org-time-stamp-inactive

    ;; Clock
    "c c" #'org-clock-cancel
    "c e" #'org-clock-modify-effort-estimate
    "c E" #'org-set-effort
    "c g" #'org-clock-goto
    "c i" #'org-clock-in
    "c o" #'org-clock-out
    "c r" #'org-clock-report)

  :config
  (org-link-set-parameters
   "mpv"
   :complete #'pkg-org/link-mpv-complete
   :follow #'pkg-org/link-mpv-follow)

  ;; Pre-load modules.
  (require 'org-agenda)
  (require 'org-tempo)

;;;; Baseline

  ;; This is currently disabled because it affects HTML exports. For example, if
  ;; index.org is exported to ./export/index.html, then links to images won't
  ;; work correctly. The code is kept here because I might create a better
  ;; solution in the future.
  ;;
  ;; (advice-add 'org-export-output-file-name :around #'pkg-org/export-output-file-name)

  ;; Store ID locations in the cache directory.
  (setq org-id-locations-file (concat my/cache-dir "org-id-locations"))

  (setq org-persist-directory (concat my/cache-dir "org-persist/"))

  ;; Don't store IDs relative to the `org-id-locations-file'.
  (setq org-id-locations-file-relative nil)

  ;; Prevent modifications made in invisible sections of an org document, as
  ;; unintended changes can easily go unseen otherwise.
  (setq org-catch-invisible-edits 'smart)

  ;; This is just a default location to look for Org files. There is no need at
  ;; all to put your files into this directory. Anyway, it should at least point
  ;; to a directory that I actually use frequently to store org files.
  (setq org-directory "~/data/repos/notes")

  (setq org-indirect-buffer-display 'current-window)

  ;; `showeverything' is org's default, but it doesn't respect
  ;; `org-hide-block-startup' (#+startup: hideblocks), archive trees, hidden
  ;; drawers, or VISIBILITY properties. `nil' is equivalent, but respects these
  ;; settings.
  (setq org-startup-folded nil)

  ;; Configure external applications for opening file:path.
  ;; "setsid -w" is needed to tell Emacs to wait until xdg-open finishes.
  (setq org-file-apps
        '((auto-mode . emacs)
          ("\\.x?html?\\'" . "setsid -w xdg-open %s")
          ("\\.mp4\\'" . "setsid -w xdg-open \"%s\"")))

  ;; Temporarily disable the echo area, because this function always displays
  ;; the message "Position saved to mark ring, go back with...".
  (advice-add #'org-mark-ring-push :around #'lib-util/mute-echo-area)

;;;; Link

  ;; Don't prompt before running code in org, just be careful.
  (setq org-link-elisp-confirm-function nil)

  ;; Prefer opening links in the current window.
  (setq org-link-frame-setup '((vm . vm-visit-folder)
                               (vm-imap . vm-visit-imap-folder)
                               (gnus . org-gnus-no-new-news)
                               (file . find-file)
                               (wl . wl)))

;;;; Src

  ;; Show edit buffer in the current window, keeping all other windows.
  (setq org-src-window-setup 'current-window)

  ;; Do not show a message in the header line of buffer.
  (setq org-edit-src-persistent-message nil)

  (setq org-blank-before-new-entry
        '((heading . auto)
          (plain-list-item . nil)))

  ;; Please, let me handle indentation.
  (setq org-edit-src-content-indentation 0
        org-src-preserve-indentation t
        org-src-tab-acts-natively t
        org-adapt-indentation nil)

;;;; Dates

  ;; Overlay custom formats over all time stamps.
  (setq org-display-custom-times t)

  (setq org-time-stamp-custom-formats '("<%Y-%m-%d %a>" . "<%Y-%m-%d %a %H:%M>"))

  ;; Change minutes in steps of 1 minute.
  (setq org-time-stamp-rounding-minutes '(0 1))

;;;; Images

  ;; When set to nil, try to get the width from an #+ATTR.* keyword and fall back
  ;; on the original width if none is found.
  (setq org-image-actual-width nil)

;;;; Logs

  (setq org-log-into-drawer t
        ;; This variable has no effect because `org-log-into-drawer' is set to t.
        org-log-state-notes-insert-after-drawers nil)

  ;; Don't log when done because state changes are already recorded.
  (setq org-log-done nil
        org-log-done-with-time t)

  (setq org-log-redeadline 'time)
  (setq org-log-reschedule 'time)
  (setq org-log-states-order-reversed t)

;;;; Tasks

  (setq org-enforce-todo-dependencies t)

  (setq org-priority-faces
        '((?A . error)
          (?B . warning)
          (?C . success)))

  (setq org-todo-keyword-faces
        '(("CANCELLED" . org-archived)
          ("DEFERRED" . org-archived)
          ("DELEGATED" . org-archived)
          ("DOING" . org-scheduled-today)
          ("DONE" . org-done)
          ("TODO" . org-todo)
          ("WAITING" . org-upcoming-deadline)))

  ;; The exclamation mark following the shortcut letter tells Org to record to
  ;; change to the new state.
  (setq org-todo-keywords
        '((sequence
           "TODO(t)"
           "STARTED(s!)"
           "WAITING(w!)"
           "|"
           "DEFERRED(!)"
           "CANCELLED(c!)"
           "DONE(d!)")))

  ;; Expert is like auto, but no special window with the keyword will be shown,
  ;; choices will only be listed in the prompt.
  (setq org-use-fast-todo-selection 'expert)

;;;; Tags

  ;; Always use fast selection. Selection keys are assigned automatically if
  ;; necessary.
  (setq org-use-fast-tag-selection t)

  ;; Place tags directly after headline text, with only one space in between.
  (setq org-tags-column 0)

  ;; Sort tags alphabetically.
  (setq org-tags-sort-function #'string-collate-lessp)

;;;; Agenda

  (setq org-agenda-files (list (file-truename "~/data/repos/notes/20200827220222.org") ; Tasks
                               (file-truename "~/data/repos/notes/20220403231051.org") ; Archive
                               ))

  (setq org-agenda-inhibit-startup t
        org-agenda-skip-unavailable-files t
        org-agenda-span 10
        org-agenda-start-day "-3d"
        org-agenda-window-setup 'current-window
        ;; Start on Monday
        org-agenda-start-on-weekday 1)

  ;; Different colors for different priority levels
  (setq org-agenda-deadline-faces
        '((1.001 . error)
          (1.0 . org-warning)
          (0.5 . org-upcoming-deadline)
          (0.0 . org-upcoming-distant-deadline)))

;;;; Archive/refile

  ;; Archive trees to a separate file.
  (setq org-archive-location (concat (file-truename "~/data/repos/notes/20220403231051.org")
                                     "::* From [[file:%s]]"))

  ;; Default is just 'from-org.
  (setq org-archive-subtree-save-file-p t)

  ;; Save all available context to archived trees.
  (setq org-archive-save-context-info '(category
                                        file
                                        itags
                                        ltags
                                        olpath
                                        time
                                        todo))

  ;; Move instead of copying.
  (setq org-refile-keep nil)

  ;; My refile targets are currently hardcoded, so there's no need to optimize
  ;; the calculation of the refile targets.
  (setq org-refile-use-cache nil)

  (setq org-refile-use-outline-path nil)

;;;; Org capture

;;;;; Templates

  (defun pkg-org-capture/note-new-path ()
    (file-truename (format "~/data/repos/notes/%s.org" (pkg-org/note-id))))

  (defvar pkg-org-capture/template-note
    (string-join (list ":properties:"
                       ":id: %(lib-util/uuid)"
                       ":end:"
                       "#+setupfile: setup.org"
                       "#+title: Note%?\n")
                 "\n"))

  (defvar pkg-org-capture/template-todo
    (string-join (list "* TODO %^{Description}"
                       ":properties:"
                       ":id: %(lib-util/uuid)"
                       ":created: %T"
                       ":end:")
                 "\n"))

;;;; UI

  ;; List of symbols corresponding to keywords to be hidden in the Org buffer.
  (setq org-hidden-keywords nil)

  ;; Enable more font lock faces.
  (setq org-fontify-done-headline t
        org-fontify-emphasized-text t
        org-fontify-quote-and-verse-blocks t
        org-fontify-todo-headline t
        org-fontify-whole-block-delimiter-line t
        org-fontify-whole-heading-line t)

  ;; Braces are required in order to trigger interpretations as sub/superscript.
  ;; This can be helpful in documents that need "_" frequently in plain text.
  ;; This also eliminates ambiguity issues.
  (setq org-use-sub-superscripts '{})

  (setq org-entities-user
        '(("flat"  "\\flat" nil "" "" "266D" "♭")
          ("sharp" "\\sharp" nil "" "" "266F" "♯")))

  ;; Allow multiline Org emphasis markup. Default is just one newline.
  ;; http://emacs.stackexchange.com/a/13828/115
  (setcar (nthcdr 4 org-emphasis-regexp-components) 5)
  (org-set-emph-re 'org-emphasis-regexp-components org-emphasis-regexp-components)

  (advice-add #'org-backward-heading-same-level :after #'pkg-org/recenter)
  (advice-add #'org-forward-heading-same-level :after #'pkg-org/recenter)

;;;; Babel

  (setq org-confirm-babel-evaluate nil)

  ;; Allow running/highlighting code blocks in org.
  (org-babel-do-load-languages
   'org-babel-load-languages
   '((clojure . t)
     (gnuplot . t)
     (js . t)
     (lisp . t)
     (plantuml . t)
     (python . t)
     (ruby . t)
     (scheme . t)
     (shell . t)))

  ;; Use the PlantUML executable, not jar.
  (setq org-plantuml-exec-mode 'plantuml)

  (setq org-babel-js-function-wrapper
        (string-join '("const util = require('util')"
                       "const options = {maxArrayLength: 100, maxStringLength: 1000, breakLength: 80, compact: false}"
                       "console.log(util.inspect((function () {\n%s\n})(), options))")
                     "\n"))

;;;; Integrations with other packages

  (with-eval-after-load 'evil
    ;; Add to jump list, i.e. record location prior to running the command.
    (evil-add-command-properties #'org-open-at-point :jump t)
    (evil-add-command-properties #'outline-up-heading :jump t)
    (evil-add-command-properties #'org-forward-heading-same-level :jump t)
    (evil-add-command-properties #'org-backward-heading-same-level :jump t)))

(provide 'pkg-org)
