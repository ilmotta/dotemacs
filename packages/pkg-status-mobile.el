;;; -*- lexical-binding: t; -*-

;;; Commentary:
;; Basic Emacs layer to more easily control a running instance of Status Mobile.
;; Requires CIDER and a running REPL.

;;; Code:

(require 'lib-util)
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

(defun pkg-status-mobile/set-log-level ()
  (interactive)
  (let* ((screens-by-name (seq-group-by #'pkg-status-mobile/-screen-completion pkg-status-mobile/preview-screens))
         (level (completing-read "Log level: " '("Debug" "Info" "Error") nil t))
         (form (format "(rf/dispatch [:log-level.ui/change-log-level-confirmed \"%s\"])" (upcase level))))
    (cider-interactive-eval form)))

(defvar pkg-status-mobile--references nil
  "Store references, for debugging purposes.")

(defvar pkg-status-mobile--debug-p nil
  "When non-nil, enable debugging.")

(defun pkg-status-mobile/re-frame-find-file-from-ref (reference keyword-to-find curr-point-marker)
  (let* ((path (replace-regexp-in-string "file://" "" (map-elt reference :uri)))
         (line-number (map-nested-elt reference [:range :start :line]))
         (line-column (map-nested-elt reference [:range :start :character]))
         (open-file-at-reg-call (lambda ()
                                  (find-file path)
                                  (goto-char (point-min))
                                  (forward-line line-number)
                                  (forward-char line-column)
                                  (recenter-top-bottom)
                                  (evil-set-jump)
                                  (xref-push-marker-stack curr-point-marker)
                                  (throw 'found nil))))
    (with-temp-buffer
      (insert-file-contents path)
      (clojure-mode)
      (goto-char (point-min))
      (forward-line line-number)
      (forward-char line-column)
      (when (beginning-of-defun)
        (paredit-forward-down)
        (let ((thing (thing-at-point 'symbol 'no-properties)))
          (cond
           ((string-equal "rf/defn" thing)
            ;; Cursor will be at the end of the (optional) docstring closing
            ;; double quotes or at the end of a line like {:events XYZ}, or the
            ;; body of the event handler, in which case no match will be found.
            (paredit-forward 3)
            (backward-char)
            (if (clojure--in-string-p) ; at docstring
                (progn
                  (forward-line)
                  (beginning-of-line)
                  (when (search-forward (concat "{:events [" keyword-to-find "]") (line-end-position) t)
                    (funcall open-file-at-reg-call)))
              (beginning-of-line)
              (when (search-forward (concat "{:events [" keyword-to-find "]") (line-end-position) t)
                (funcall open-file-at-reg-call))))

           ((or (string-match-p "rf/reg-.*" thing)
                (string-match-p "re-frame/reg-.*" thing)
                (string-match-p "reg-root-key-sub" thing))
            (paredit-forward 2)
            (when (string-equal keyword-to-find (thing-at-point 'symbol 'no-properties))
              (funcall open-file-at-reg-call)))))))))

(defun pkg-status-mobile/re-frame-find-references-raw ()
  (let ((exclude-declaration nil))
    (lsp-request "textDocument/references"
                 (append (lsp--text-document-position-params)
                         (list :context
                               `(:includeDeclaration ,(lsp-json-bool (not (or exclude-declaration lsp-references-exclude-definition)))))))))

(defun pkg-status-mobile/re-frame-find-registration ()
  (interactive)
  (let ((keyword-to-find (thing-at-point 'symbol 'no-properties))
        (references (pkg-status-mobile/re-frame-find-references-raw))
        (curr-point-marker (point-marker)))
    (when pkg-status-mobile--debug-p
      (setq pkg-status-mobile--references references))
    (catch 'found
      (seq-doseq (ref references)
        (pkg-status-mobile/re-frame-find-file-from-ref ref keyword-to-find curr-point-marker)))))

(defun pkg-status-mobile/do-login (nrepl-response)
  (let* ((raw-val (nrepl-dict-get nrepl-response "value"))
         (val (if (string= "nil" raw-val) nil raw-val)))
    (when val
      (let* ((response (json-read-from-string (json-read-from-string val)))
             (accounts (seq-map (lambda (account)
                                  (cons (map-elt account 'name)
                                        (map-elt account 'key-uid)))
                                response))
             (account (if (= 1 (length accounts))
                          (seq-first accounts)
                        (completing-read "account to login:" accounts))))
        (cider-interactive-eval (format "
(swap! re-frame.db/app-db assoc-in
  [:profile/login :password]
  (utils.security.core/mask-data \"%s\"))
(swap! re-frame.db/app-db assoc-in
  [:profile/login :key-uid]
  (:key-uid (utils.re-frame/sub [:profile/login-profile])))
(utils.re-frame/dispatch [:profile.login/login])
"
                                        "passwordss"))))))

(defun pkg-status-mobile/login ()
  (interactive)
  (cider-interactive-eval "
(->> (utils.re-frame/sub [:profile/profiles-overview])
     vals
     (sort-by :timestamp)
     reverse
     clj->js
     js/JSON.stringify)
" #'pkg-status-mobile/do-login))

(defun pkg-status-mobile/logout ()
  (interactive)
  (let ((form "(utils.re-frame/dispatch [:logout])"))
    (cider-interactive-eval form)))

(transient-define-prefix pkg-status-mobile/main-t ()
  :transient-non-suffix #'transient--do-quit-one
  [[:description "Session"
    ("l" "Login" pkg-status-mobile/login)
    ("x" "Logout" pkg-status-mobile/logout :transient t)]
   [:description "Navigation"
    :pad-keys t
    ("g" "Go to screen" pkg-status-mobile/navigate-to :transient t)]])

(provide 'pkg-status-mobile)
