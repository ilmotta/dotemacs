;;; -*- lexical-binding: t; -*-

;;; Commentary:
;; Basic Emacs layer to more easily control a running instance of Status Mobile.
;; Requires CIDER and a running REPL.

;;; Code:

(require 'notifications)

(defvar pkg-status-mobile/preview-screens
  '((:name "account-avatar" :category "avatar")
    (:name "account-selector" :category "tabs")
    (:name "accounts" :category "settings")
    (:name "action-drawers" :category "drawers")
    (:name "activity-logs" :category "notifications")
    (:name "activity-logs-photos" :category "notifications")
    (:name "animated-header-list" :category "animated-list")
    (:name "author" :category "messages")
    (:name "banner" :category "banner")
    (:name "bottom-nav-tab" :category "navigation")
    (:name "browser-input" :category "browser")
    (:name "button" :category "buttons")
    (:name "calendar" :category "calendar")
    (:name "calendar-day" :category "calendar")
    (:name "calendar-year" :category "calendar")
    (:name "category" :category "settings")
    (:name "channel" :category "list-items")
    (:name "channel-actions" :category "community")
    (:name "channel-avatar" :category "avatar")
    (:name "collectible" :category "profile")
    (:name "color-picker" :category "colors")
    (:name "community-card-view" :category "community")
    (:name "community-list" :category "list-items")
    (:name "community-membership-list-view" :category "community")
    (:name "context-tags" :category "tags")
    (:name "counter" :category "counter")
    (:name "disclaimer" :category "selectors")
    (:name "discover-card" :category "community")
    (:name "divider-date" :category "dividers")
    (:name "divider-label" :category "dividers")
    (:name "documentation-drawer" :category "drawers")
    (:name "drawer-buttons" :category "drawers")
    (:name "dropdown" :category "dropdowns")
    (:name "dynamic-button" :category "buttons")
    (:name "empty-state" :category "empty-state")
    (:name "filter" :category "selectors")
    (:name "floating-shell-button" :category "navigation")
    (:name "gap" :category "messages")
    (:name "group-avatar" :category "avatar")
    (:name "icon-avatar" :category "avatar")
    (:name "info-message" :category "info")
    (:name "information-box" :category "info")
    (:name "input" :category "inputs")
    (:name "keycard-component" :category "keycard")
    (:name "link-preview" :category "links")
    (:name "lowest-price" :category "wallet")
    (:name "markdown-list" :category "markdown")
    (:name "network-amount" :category "wallet")
    (:name "network-breakdown" :category "wallet")
    (:name "new-messages" :category "dividers")
    (:name "notification" :category "notifications")
    (:name "page-nav" :category "navigation")
    (:name "permission-drawers" :category "drawers")
    (:name "permission-tag" :category "tags")
    (:name "predictive-keyboard" :category "buttons")
    (:name "preview-lists" :category "list-items")
    (:name "privacy-option" :category "settings")
    (:name "profile-card" :category "profile")
    (:name "profile-input" :category "inputs")
    (:name "qr-code" :category "share")
    (:name "react" :category "reactions")
    (:name "record-audio" :category "record-audio")
    (:name "recovery-phrase-input" :category "inputs")
    (:name "reorder-item" :category "settings")
    (:name "search-input" :category "inputs")
    (:name "segmented" :category "tabs")
    (:name "select-profile" :category "profile")
    (:name "select-reactions" :category "selectors")
    (:name "selectors" :category "selectors")
    (:name "settings-list" :category "settings")
    (:name "shadows" :category "foundations")
    (:name "share-qr-code" :category "share")
    (:name "skeleton" :category "loaders")
    (:name "slide-button" :category "buttons")
    (:name "small-option-card" :category "onboarding")
    (:name "snippet" :category "code")
    (:name "status-tags" :category "tags")
    (:name "step" :category "counter")
    (:name "strength-divider" :category "dividers")
    (:name "switcher-cards" :category "switcher")
    (:name "system-messages" :category "messages")
    (:name "tabs" :category "tabs")
    (:name "tags" :category "tags")
    (:name "texts" :category "markdown")
    (:name "tips" :category "password")
    (:name "title" :category "text-combinations")
    (:name "title-input" :category "inputs")
    (:name "toast" :category "notifications")
    (:name "token-gating" :category "community")
    (:name "token-overview" :category "wallet")
    (:name "token-tag" :category "tags")
    (:name "top-nav" :category "navigation")
    (:name "url-preview" :category "links")
    (:name "url-preview-list" :category "links")
    (:name "user-avatar" :category "avatar")
    (:name "user-list" :category "list-items")
    (:name "wallet-user-avatar" :category "avatar"))
  "Preview screens.

Use the following ClojureScript snippet to get the updated list
of plists:

  (mapcat (fn [[category screens]]
            (map (fn [screen]
                   (list :name (name (:name screen))
                         :category (name category)))
                 screens))
          status-im2.contexts.quo-preview.main/screens-categories)
")

(defun pkg-status-mobile/-screen-completion (screen)
  (concat (map-elt screen :category)
          " > "
          (map-elt screen :name)))

(defun pkg-status-mobile/-screens->completions ()
  (seq-map (lambda (screen)
             (cons (pkg-status-mobile/-screen-completion screen)
                   screen))
           pkg-status-mobile/preview-screens))

(defun pkg-status-mobile/navigate-to ()
  (interactive)
  (let* ((screens-by-name (seq-group-by #'pkg-status-mobile/-screen-completion pkg-status-mobile/preview-screens))
         (completion (completing-read "Screen: " (pkg-status-mobile/-screens->completions) nil t))
         (screen (thread-first screens-by-name
                               (map-elt completion)
                               (seq-first)))
         (screen-name (map-elt screen :name))
         (form-builder (map-elt screen :form))
         (form (cond (form-builder (funcall form-builder screen-name))
                     (:else (format "(re-frame.core/dispatch [:navigate-to :%s])"
                                    screen-name)))))
    (cider-interactive-eval form)))

(defun pkg-status-mobile/navigate-back ()
  (interactive)
  (cider-interactive-eval "(re-frame.core/dispatch [:navigate-back])"))

(defun pkg-status-mobile/open-all-projects ()
  (interactive)
  (let ((tab-names (pkg-tab-bar/-tab-names)))
    (unless (member "status-mobile" tab-names)
      (pkg-tab-bar/switch-project-as-tab "~/data/repos/status/mobile/status-mobile/"))
    (unless (member "status-go" tab-names)
      (pkg-tab-bar/switch-project-as-tab "~/data/repos/status/status-go/status-go/"))))

(defun pkg-status-mobile/start-react-native-android ()
  (let ((proc (start-process "run-android" "*status-mobile-run-android*" "make" "run-android")))
    (set-process-sentinel
     proc
     (lambda (process event)
       (when (string-prefix-p "finished" (substring event 0 8))
         (notifications-notify :title "Status Mobile"
                               :body "Emulated Android app is running."
                               :urgency 'low
                               :suppress-sound t))))))

(defun pkg-status-mobile/start-react-native-android ()
  (let ((proc (start-process "run-android" "*status-mobile-run-android*" "make" "run-android")))
    (set-process-sentinel
     proc
     (lambda (process event)
       (when (string-prefix-p "finished" (substring event 0 8))
         (notifications-notify :title "Status Mobile"
                               :body "Emulated Android app is running."
                               :urgency 'normal
                               :suppress-sound t))))))

(defun pkg-status-mobile/start-metro ()
  (let ((proc (start-process "run-metro" "*status-mobile-run-metro*" "make" "run-metro" "TARGET=android")))
    (set-process-filter
     proc
     (lambda (process output)
       (with-current-buffer (process-buffer process)
         (insert output)
         (ansi-color-apply-on-region (point-min) (point-max))
         (goto-char (point-max))
         (when (string-match-p "Welcome to Metro!" output)
           (message "[info][status] Metro finished.")
           (process-send-eof process)
           (pkg-status-mobile/start-react-native-android)))))))

(defun pkg-status-mobile/start-shadowcljs-target-mobile ()
  (let ((proc (start-process "run-clojure" "*status-mobile-run-clojure*" "make" "run-clojure")))
    (set-process-filter
     proc
     (lambda (process output)
       (with-current-buffer (process-buffer process)
         (insert output)
         (ansi-color-apply-on-region (point-min) (point-max))
         (goto-char (point-max))
         (when (string-match-p (rx "[:mobile] Build completed.") output)
           (message "[info][status] Shadow-CLJS :mobile target finished.")
           (process-send-eof process)
           (pkg-status-mobile/start-metro)))))))

(defun pkg-status-mobile/start-app ()
  (interactive)
  (let ((default-directory (project-root (project-current))))
    (pkg-status-mobile/start-shadowcljs-target-mobile)))

(defun pkg-status-mobile/start-shadowcljs-test-repl ()
  (let ((proc (start-process "shadow-cljs-test-repl" "*status-mobile-shadow-cljs-test-repl*"
                             "nix-shell"
                             "--show-trace"
                             "--attr" "shells.default"
                             "--keep" "default" "default.nix"
                             "--run" "yarn shadow-cljs cljs-repl test"))
        (started-p nil))
    (set-process-filter
     proc
     (lambda (process output)
       (with-current-buffer (process-buffer process)
         (insert output)
         (goto-char (point-max))
         (when (and (not started-p) (string-match-p (rx "cljs.user=>") output))
           (setq started-p t)
           (notifications-notify :title "Status Mobile"
                                 :body "Test REPL is ready!"
                                 :urgency 'normal
                                 :suppress-sound t)
           (process-send-eof process)))))))

(defun pkg-status-mobile/start-shadowcljs-compiled-tests ()
  (let ((proc (start-process "shadow-cljs-compiled-tests" "*status-mobile-shadow-cljs-compiled-tests*"
                             "nix-shell"
                             "--show-trace"
                             "--attr" "shells.default"
                             "--keep" "default" "default.nix"
                             "--run" "node --require ./test-resources/override.js target/test/test.js --repl"))
        (started-p nil))
    (set-process-filter
     proc
     (lambda (process output)
       (with-current-buffer (process-buffer process)
         (insert output)
         (goto-char (point-max))
         (when (and (not started-p) (string-match-p (rx "shadow-cljs - #" (= 1 digit) " ready!") output))
           (setq started-p t)
           (message "[info][status] Successfully executed Shadow-CLJS cljs-test.")
           (process-send-eof process)
           (pkg-status-mobile/start-shadowcljs-test-repl)))))))

(defun pkg-status-mobile/start-shadowcljs-target-test ()
  (let ((proc (start-process "shadow-cljs-watch-test" "*status-mobile-shadow-cljs-watch-test*"
                             "nix-shell"
                             "--show-trace"
                             "--attr" "shells.default"
                             "--keep" "default" "default.nix"
                             "--run" "yarn shadow-cljs watch test --verbose"))
        (started-p nil))
    (set-process-filter
     proc
     (lambda (process output)
       (with-current-buffer (process-buffer process)
         (insert output)
         (goto-char (point-max))
         (when (and (not started-p) (string-match-p (rx "[:test] Build completed.") output))
           (setq started-p t)
           (message "[info][status] Shadow-CLJS :test target finished.")
           (process-send-eof process)
           (pkg-status-mobile/start-shadowcljs-compiled-tests)))))))

(defun pkg-status-mobile/start-test-repl ()
  (interactive)
  (let ((default-directory (project-root (project-current))))
    (pkg-status-mobile/start-shadowcljs-target-test)))

(provide 'pkg-status-mobile)
