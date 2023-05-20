;;; -*- lexical-binding: t; -*-

;;; Commentary:
;; Basic Emacs layer to more easily control a running instance of Status Mobile.
;; Requires CIDER and a running REPL.

;;; Code:

(defvar pkg-status-mobile/navigation-screens
  `(;; Quo2 Preview
    (":animated-header-list" . (:category "Quo2 Preview Animations" :description "Animated header list"))
    ,@(let ((category "Quo2 Preview Avatar"))
       `((":account-avatar"     . (:category ,category :description "Account"))
         (":channel-avatar"     . (:category ,category :description "Channel"))
         (":group-avatar"       . (:category ,category :description "Group"))
         (":icon-avatar"        . (:category ,category :description "Icon"))
         (":user-avatar"        . (:category ,category :description "User"))
         (":wallet-user-avatar" . (:category ,category :description "Wallet user"))))
    (":banner" . (:category "Quo2 Preview Banner" :description "Banner"))
    ,@(let ((category "Quo2 Preview Buttons"))
       `((":button"         . (:category ,category :description "Button"))
         (":dynamic-button" . (:category ,category :description "Dynamic button"))))
    (":snippet" . (:category "Quo2 Preview Snippet" :description "Snippet"))
    ,@(let ((category "Quo2 Preview Community"))
       `((":community-card-view"            . (:category ,category :description "Card view"))
         (":community-list-view"            . (:category ,category :description "Community list view"))
         (":community-membership-list-view" . (:category ,category :description "Community membership list view"))
         (":discover-card"                  . (:category ,category :description "Discover card"))
         (":token-gating"                   . (:category ,category :description "Community token gating"))))
    (":counter" . (:category "Quo2 Preview Counter" :description "Counter"))
    ,@(let ((category "Quo2 Preview Dividers"))
       `((":divider-date"  . (:category ,category :description "Divider date"))
         (":divider-label" . (:category ,category :description "Divider label"))
         (":new-messages"  . (:category ,category :description "New messages"))))
    ,@(let ((category "Quo2 Preview Tabs"))
       `((":account-selector" . (:category ,category :description "Account selector"))
         (":segmented"        . (:category ,category :description "Segmented"))
         (":tabs"             . (:category ,category :description "Tabs"))))

    ;; Screens
    (":quo2-preview"         . (:category "Quo2.0 Preview"))
    (":activity-center/open" . (:category "Activity Center"
                                :form (lambda (name)
                                        (format "(re-frame.core/dispatch [%s])" name)))))
  "List of all supported screens.")

(defun pkg-status-mobile/navigate-to ()
  (interactive)
  (let* ((completion-extra-properties
          '(:affixation-function (lambda (completions)
                                   (seq-map
                                    (lambda (completion)
                                      (let* ((screen (map-elt pkg-status-mobile/navigation-screens completion))
                                             (prefix (map-elt screen :category))
                                             (suffix completion)
                                             (candidate (concat "  " (map-elt screen :description) "  ")))
                                       (list
                                        (string-pad candidate 20)
                                        (string-pad (propertize prefix 'face 'completions-common-part) 25)
                                        (propertize suffix 'face 'completions-annotations))))
                                    completions))))
         (screen-name (completing-read "Screen: " pkg-status-mobile/navigation-screens nil t))
         (form-builder (thread-first pkg-status-mobile/navigation-screens
                                     (map-elt screen-name)
                                     (map-elt :form)))
         (form (cond (form-builder (funcall form-builder screen-name))
                     (:else (format "(re-frame.core/dispatch [:navigate-to %s])" screen-name)))))
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

(provide 'pkg-status-mobile)
