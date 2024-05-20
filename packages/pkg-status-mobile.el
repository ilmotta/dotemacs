;;; -*- lexical-binding: t; -*-

;;; Commentary:
;; Basic Emacs layer to more easily control a running instance of Status Mobile.
;; Requires CIDER and a running REPL.

;;; Code:

(require 'lib-util)
(require 'notifications)

(defvar pkg-status-mobile/preview-screens
  '((:name "animated-header-list" :category "animated-list")
    (:name "account-avatar" :category "avatar")
    (:name "channel-avatar" :category "avatar")
    (:name "collection-avatar" :category "avatar")
    (:name "group-avatar" :category "avatar")
    (:name "icon-avatar" :category "avatar")
    (:name "user-avatar" :category "avatar")
    (:name "wallet-user-avatar" :category "avatar")
    (:name "banner" :category "banner")
    (:name "browser-input" :category "browser")
    (:name "button" :category "buttons")
    (:name "composer-button" :category "buttons")
    (:name "dynamic-button" :category "buttons")
    (:name "predictive-keyboard" :category "buttons")
    (:name "slide-button" :category "buttons")
    (:name "wallet-button" :category "buttons")
    (:name "wallet-ctas" :category "buttons")
    (:name "calendar" :category "calendar")
    (:name "calendar-day" :category "calendar")
    (:name "calendar-year" :category "calendar")
    (:name "snippet" :category "code")
    (:name "snippet-preview" :category "code")
    (:name "color" :category "colors")
    (:name "color-picker" :category "colors")
    (:name "channel-action" :category "community")
    (:name "channel-actions" :category "community")
    (:name "community-card-view" :category "community")
    (:name "community-detail-token-gating" :category "community")
    (:name "community-membership-list-view" :category "community")
    (:name "community-stat" :category "community")
    (:name "discover-card" :category "community")
    (:name "token-gating" :category "community")
    (:name "collectible-counter" :category "counter")
    (:name "counter" :category "counter")
    (:name "step" :category "counter")
    (:name "divider-date" :category "dividers")
    (:name "divider-label" :category "dividers")
    (:name "divider-line" :category "dividers")
    (:name "new-messages" :category "dividers")
    (:name "strength-divider" :category "dividers")
    (:name "action-drawers" :category "drawers")
    (:name "bottom-actions" :category "drawers")
    (:name "documentation-drawer" :category "drawers")
    (:name "drawer-action" :category "drawers")
    (:name "drawer-buttons" :category "drawers")
    (:name "drawer-top" :category "drawers")
    (:name "permission-drawers" :category "drawers")
    (:name "dropdown" :category "dropdowns")
    (:name "dropdown-input" :category "dropdowns")
    (:name "network-dropdown" :category "dropdowns")
    (:name "empty-state" :category "empty-state")
    (:name "gradients" :category "foundations")
    (:name "shadows" :category "foundations")
    (:name "gradient-cover" :category "gradient")
    (:name "interactive-graph" :category "graph")
    (:name "wallet-graph" :category "graph")
    (:name "info-message" :category "info")
    (:name "information-box" :category "info")
    (:name "address-input" :category "inputs")
    (:name "input" :category "inputs")
    (:name "locked-input" :category "inputs")
    (:name "profile-input" :category "inputs")
    (:name "recovery-phrase-input" :category "inputs")
    (:name "search-input" :category "inputs")
    (:name "title-input" :category "inputs")
    (:name "drawer-bar" :category "ios")
    (:name "keycard-component" :category "keycard")
    (:name "internal-link-card" :category "links")
    (:name "link-preview" :category "links")
    (:name "url-preview" :category "links")
    (:name "url-preview-list" :category "links")
    (:name "account" :category "list-items")
    (:name "account-list-card" :category "list-items")
    (:name "address" :category "list-items")
    (:name "channel" :category "list-items")
    (:name "community-list" :category "list-items")
    (:name "dapp" :category "list-items")
    (:name "network-list" :category "list-items")
    (:name "preview-lists" :category "list-items")
    (:name "quiz-item" :category "list-items")
    (:name "saved-address" :category "list-items")
    (:name "saved-contact-address" :category "list-items")
    (:name "token-network" :category "list-items")
    (:name "token-value" :category "list-items")
    (:name "user-list" :category "list-items")
    (:name "skeleton-list" :category "loaders")
    (:name "markdown-list" :category "markdown")
    (:name "texts" :category "markdown")
    (:name "author" :category "messages")
    (:name "gap" :category "messages")
    (:name "system-messages" :category "messages")
    (:name "bottom-nav-tab" :category "navigation")
    (:name "floating-shell-button" :category "navigation")
    (:name "page-nav" :category "navigation")
    (:name "top-nav" :category "navigation")
    (:name "activity-logs" :category "notifications")
    (:name "activity-logs-photos" :category "notifications")
    (:name "notification" :category "notifications")
    (:name "toast" :category "notifications")
    (:name "keyboard-key" :category "numbered-keyboard")
    (:name "numbered-keyboard" :category "numbered-keyboard")
    (:name "small-option-card" :category "onboarding")
    (:name "password-tips" :category "password")
    (:name "tips" :category "password")
    (:name "collectible" :category "profile")
    (:name "collectible-list-item" :category "profile")
    (:name "expanded-collectible" :category "profile")
    (:name "link-card" :category "profile")
    (:name "profile-card" :category "profile")
    (:name "select-profile" :category "profile")
    (:name "showcase-nav" :category "profile")
    (:name "record-audio" :category "record-audio")
    (:name "disclaimer" :category "selectors")
    (:name "filter" :category "selectors")
    (:name "react" :category "selectors")
    (:name "react-selector" :category "selectors")
    (:name "reactions-selector" :category "selectors")
    (:name "selectors" :category "selectors")
    (:name "accounts" :category "settings")
    (:name "category" :category "settings")
    (:name "data-item" :category "settings")
    (:name "page-setting" :category "settings")
    (:name "privacy-option" :category "settings")
    (:name "reorder-item" :category "settings")
    (:name "section-label" :category "settings")
    (:name "settings-item" :category "settings")
    (:name "qr-code" :category "share")
    (:name "share-qr-code" :category "share")
    (:name "group-messaging-card" :category "switchers")
    (:name "switcher-cards" :category "switchers")
    (:name "account-selector" :category "tabs")
    (:name "segmented" :category "tabs")
    (:name "tabs" :category "tabs")
    (:name "collectible-tag" :category "tags")
    (:name "context-tags" :category "tags")
    (:name "network-tags" :category "tags")
    (:name "number-tag" :category "tags")
    (:name "permission-tag" :category "tags")
    (:name "status-tags" :category "tags")
    (:name "summary-tag" :category "tags")
    (:name "tag" :category "tags")
    (:name "tags" :category "tags")
    (:name "tiny-tag" :category "tags")
    (:name "token-tag" :category "tags")
    (:name "channel-name" :category "text-combinations")
    (:name "page-top" :category "text-combinations")
    (:name "standard-title" :category "text-combinations")
    (:name "text-combinations" :category "text-combinations")
    (:name "username" :category "text-combinations")
    (:name "account-card" :category "wallet")
    (:name "account-origin" :category "wallet")
    (:name "account-overview" :category "wallet")
    (:name "account-permissions" :category "wallet")
    (:name "amount-input" :category "wallet")
    (:name "confirmation-progress" :category "wallet")
    (:name "keypair" :category "wallet")
    (:name "network-amount" :category "wallet")
    (:name "network-bridge" :category "wallet")
    (:name "network-link" :category "wallet")
    (:name "network-routing" :category "wallet")
    (:name "progress-bar" :category "wallet")
    (:name "required-tokens" :category "wallet")
    (:name "summary-info" :category "wallet")
    (:name "token-input" :category "wallet")
    (:name "transaction-progress" :category "wallet")
    (:name "transaction-summary" :category "wallet")
    (:name "wallet-activity" :category "wallet")
    (:name "wallet-overview" :category "wallet"))
  "Preview screens.

Use the following ClojureScript snippet to get the updated list
of plists:

  (->> status-im.contexts.preview.quo.main/screens-categories
       (mapcat (fn [[category screens]]
                 (map (fn [screen]
                        {:name     (name (:name screen))
                         :category (name category)})
                      screens)))
       (sort-by (juxt :category :name))
       (map (fn [screen]
              (list :name     (name (:name screen))
                    :category (:category screen)))))
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
                        (completing-read "Account to login:" accounts))))
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
    ("l" "Login" pkg-status-mobile/login :transient t)
    ("x" "Logout" pkg-status-mobile/logout :transient t)]
   [:description "Navigation"
    :pad-keys t
    ("g" "Go to screen" pkg-status-mobile/navigate-to :transient t)]])

(provide 'pkg-status-mobile)
