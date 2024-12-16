;;; -*- lexical-binding: t; -*-

;;; Commentary:
;; Basic Emacs layer to more easily control a running instance of Status Mobile.
;; Requires CIDER and a running REPL.

;;; Code:

(require 'lib-util)
(require 'notifications)

(defmacro smob--clj-as-str (&rest body)
  `(prin1-to-string (quote (do ,@body))))

(defmacro smob--cider-eval (clj-form callback &rest format-args)
  `(let ((form (format (smob--clj-as-str ,clj-form)
                ,@format-args)))
    (cider-interactive-eval form ,callback)))

(defun smob-short-compressed-key (s)
  (concat (substring s 0 6) "..." (substring s -3)))

(defun smob--screen-completion (screen)
  (concat (map-elt screen 'category)
          " > "
          (map-elt screen 'name)))

(defun smob--screens->completions (screens)
  (seq-map (lambda (screen)
             (cons (smob--screen-completion screen)
                   screen))
           screens))

(defun smob-clj-form-navigate-to (screen-name)
  (format (smob--clj-as-str
           (re-frame.core/dispatch [:navigate-to :%s]))
          screen-name))

(defun smob-open-modal (modal-id f)
  (smob--cider-eval
   (re-frame.core/dispatch [:open-modal %s])
   f
   modal-id))

(defun smob--get-quo-preview-screens (f)
  (smob--cider-eval
   (->> status-im.contexts.preview.quo.main/screens-categories
        (mapcat (fn [[category screens]]
                    (map (fn [screen]
                             {:name     (name (:name screen))
                             :category (name category)})
                         screens)))
        (sort-by (juxt :category :name))
        clj->js
        js/JSON.stringify)
   (lambda (nrepl-response)
     (when-let ((raw-value (nrepl-dict-get nrepl-response "value")))
       (let* ((json-false json-null)
              (response (json-read-from-string (json-read-from-string raw-value))))
         (funcall f response))))))

(defun smob-navigate-to ()
  (interactive)
  (smob--get-quo-preview-screens
   (lambda (preview-screens)
     (let* ((screens-by-name (seq-group-by #'smob--screen-completion preview-screens))
            (completion (completing-read "Screen: " (smob--screens->completions preview-screens) nil t))
            (screen (thread-first screens-by-name
                                  (map-elt completion)
                                  (seq-first)))
            (screen-name (map-elt screen 'name))
            (form (smob-clj-form-navigate-to screen-name)))
       (cider-interactive-eval form)))))

(defun smob-navigate-back ()
  (interactive)
  (cider-interactive-eval "(re-frame.core/dispatch [:navigate-back])"))

(defun smob-open-all-projects ()
  (interactive)
  (let ((tab-names (pkg-tab-bar/-tab-names)))
    (unless (member "status-mobile" tab-names)
      (pkg-tab-bar/switch-project-as-tab "~/data/repos/status/mobile/status-mobile/"))
    (unless (member "status-go" tab-names)
      (pkg-tab-bar/switch-project-as-tab "~/data/repos/status/status-go/status-go/"))))

(defun smob-start-react-native-android ()
  (let ((proc (start-process "run-android" "*status-mobile-run-android*" "make" "run-android")))
    (set-process-sentinel
     proc
     (lambda (process event)
       (when (string-prefix-p "finished" (substring event 0 8))
         (notifications-notify :title "Status Mobile"
                               :body "Emulated Android app is running."
                               :urgency 'low
                               :suppress-sound t))))))

(defun smob-start-react-native-android ()
  (let ((proc (start-process "run-android" "*status-mobile-run-android*" "make" "run-android")))
    (set-process-sentinel
     proc
     (lambda (process event)
       (when (string-prefix-p "finished" (substring event 0 8))
         (notifications-notify :title "Status Mobile"
                               :body "Emulated Android app is running."
                               :urgency 'normal
                               :suppress-sound t))))))

(defun smob-start-metro ()
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
           (smob-start-react-native-android)))))))

(defun smob-start-shadowcljs-target-mobile ()
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
           (smob-start-metro)))))))

(defun smob-start-app ()
  (interactive)
  (let ((default-directory (project-root (project-current))))
    (smob-start-shadowcljs-target-mobile)))

(defun smob-start-shadowcljs-test-repl ()
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

(defun smob-start-shadowcljs-compiled-tests ()
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
           (smob-start-shadowcljs-test-repl)))))))

(defun smob-start-shadowcljs-target-test ()
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
           (smob-start-shadowcljs-compiled-tests)))))))

(defun smob-start-test-repl ()
  (interactive)
  (let ((default-directory (project-root (project-current))))
    (smob-start-shadowcljs-target-test)))

(defun smob-set-log-level ()
  (interactive)
  (let* ((screens-by-name (seq-group-by #'smob--screen-completion smob-preview-screens))
         (level (completing-read "Log level: " '("Debug" "Info" "Error") nil t))
         (form (format (smob--clj-as-str
                        (rf/dispatch [:log-level.ui/change-log-level-confirmed "%s"]))
                       (upcase level))))
    (cider-interactive-eval form)))

(defvar smob--references nil
  "Store references, for debugging purposes.")

(defvar smob--debug-p nil
  "When non-nil, enable debugging.")

(defun smob-re-frame-find-file-from-ref (reference keyword-to-find curr-point-marker)
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

(defun smob-re-frame-find-references-raw ()
  (let ((exclude-declaration nil))
    (lsp-request "textDocument/references"
                 (append (lsp--text-document-position-params)
                         (list :context
                               `(:includeDeclaration ,(lsp-json-bool (not (or exclude-declaration lsp-references-exclude-definition)))))))))

(defun smob-re-frame-find-registration ()
  (interactive)
  (let ((keyword-to-find (thing-at-point 'symbol 'no-properties))
        (references (smob-re-frame-find-references-raw))
        (curr-point-marker (point-marker)))
    (when smob--debug-p
      (setq smob--references references))
    (catch 'found
      (seq-doseq (ref references)
        (smob-re-frame-find-file-from-ref ref keyword-to-find curr-point-marker)))))

(defun smob-do-toggle-feature-flag (nrepl-response)
  (let* ((raw-val (nrepl-dict-get nrepl-response "value"))
         (val (if (string= "nil" raw-val) nil raw-val)))
    (when val
      (let* ((json-false json-null)
             (flags (json-read-from-string (json-read-from-string val)))
             (flag (completing-read "Feature flags: " flags)))
        (smob--cider-eval
         (status-im.feature-flags/toggle
          (keyword (str "status-im.feature-flags/" "%s")))
         nil
         flag)))))

(defun smob-toggle-feature-flag ()
  (interactive)
  (smob--cider-eval
   (js/JSON.stringify (clj->js (status-im.feature-flags/feature-flags)))
   #'smob-do-toggle-feature-flag))

(defun smob-do-login (nrepl-response)
  (let* ((raw-val (nrepl-dict-get nrepl-response "value"))
         (val (if (string= "nil" raw-val) nil raw-val)))
    (when val
      (let* ((response (json-read-from-string (json-read-from-string val)))
             (accounts (seq-map (lambda (account)
                                  (cons (map-elt account 'name)
                                        (map-elt account 'key-uid)))
                                response))
             (account-choice (if (= 1 (length accounts))
                                 (car (seq-first accounts))
                               (completing-read "Account to login:" accounts)))
             (account (seq-find (lambda (account)
                                  (string= account-choice (car account)))
                                accounts))
             (key-uid (cdr account)))
        (smob--cider-eval
         (do (swap! re-frame.db/app-db assoc-in
                    [:profile/login]
                    {:password (utils.security.core/mask-data "%s")
                    :key-uid  "%s"})
             (utils.re-frame/dispatch [:profile.login/login])
           :ok)
         nil
         "passwordss"
         key-uid)))))

(defun smob-login ()
  (interactive)
  (cider-interactive-eval
   (smob--clj-as-str
    (->> (utils.re-frame/sub [:profile/profiles-overview])
         vals
         (sort-by :timestamp)
         reverse
         clj->js
         js/JSON.stringify))
   #'smob-do-login))

(defun smob-create-account (display-name)
  (interactive "sName: ")
  (smob--cider-eval
   (do
       ;; You are new to status, no accounts yet.
       (when-let [f @status-im.contexts.onboarding.common.overlay.view/blur-show-fn-atom]
         (f))

       ;; You are on the list of accounts screen.
       (when-let [f @status-im.contexts.profile.profiles.view/push-animation-fn-atom]
         (f))

     (utils.re-frame/dispatch [:open-modal :screen/onboarding.new-to-status])
     (utils.re-frame/dispatch [:onboarding/navigate-to-create-profile])
     (utils.re-frame/dispatch [:onboarding/profile-data-set {:display-name "%s" :color :blue}])
     (utils.re-frame/dispatch [:onboarding/password-set (utils.security.core/mask-data "passwordss")]))
   nil
   display-name))

(defun smob-copy-compressed-key ()
  (interactive)
  (smob--cider-eval
   (rf/sub [:profile/compressed-key])
   (lambda (nrepl-response)
     (when-let ((raw-val (nrepl-dict-get nrepl-response "value")))
       (let ((val (car (read-from-string raw-val))))
         (kill-new val)
         (message "Copied compressed key '%s'" (smob-short-compressed-key val)))))))

(defun smob-open-settings ()
  (interactive)
  (smob-open-modal :settings nil))

(defun smob-open-advanced-settings ()
  (interactive)
  (smob-open-modal :legacy-advanced-settings nil))

(defun smob-open-feature-flags ()
  (interactive)
  (smob-open-modal :feature-flags nil))

(defun smob-open-edit-profile ()
  (interactive)
  (smob-open-modal :edit-profile nil))

(defun smob-logout ()
  (interactive)
  (smob--cider-eval
   (do (utils.re-frame/dispatch [:profile/logout])
       :ok)
   nil))

(transient-define-prefix smob-transient-login ()
  :transient-non-suffix #'transient--do-quit-one
  [:description "Session"
   ("l" "Login" smob-login)
   ("x" "Logout" smob-logout)
   ("c" "Create account" smob-create-account)])

(transient-define-prefix smob-transient-navigation ()
  :transient-non-suffix #'transient--do-quit-one
  [:description "Navigation"
   :pad-keys t
   ("s" "Settings" smob-open-settings)
   ("S" "Advanced settings" smob-open-advanced-settings)
   ("F" "Feature flags" smob-open-feature-flags)
   ("g" "Go to screen" smob-navigate-to)])

(transient-define-prefix smob-transient-profile ()
  :transient-non-suffix #'transient--do-quit-one
  [:description "Profile"
   ("e" "Edit profile" smob-open-edit-profile)
   ("c" "Copy compressed key" smob-copy-compressed-key)])

(transient-define-prefix smob-transient-main ()
  :transient-non-suffix #'transient--do-quit-one
  [[:description "Common operations"
    ("l" "Login" smob-login)
    ("x" "Logout" smob-logout)
    ("g" "Go to screen" smob-navigate-to)]
   [("s" "Session" smob-transient-login)
    ("n" "Navitation" smob-transient-navigation)
    ("p" "Profile" smob-transient-profile)
    ("t" "Toggle feature flag" smob-toggle-feature-flag)]])

(provide 'pkg-status-mobile)
