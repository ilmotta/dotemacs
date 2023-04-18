;;; -*- lexical-binding: t; -*-

(defun pkg-tab-bar/select-tab-action (tab-name)
  "Switch to TAB-NAME or create a new tab using TAB-NAME."
  (if (tab-bar--tab-index-by-name tab-name)
      (tab-bar-switch-to-tab tab-name)
    (tab-bar-new-tab)
    (tab-bar-rename-tab tab-name)))


(defun pkg-tab-bar/-tab-names ()
  (thread-last (tab-bar-tabs)
               (seq-map (lambda (tab)
                          (alist-get 'name tab)))))

(defun pkg-tab-bar/select-tab-dwim ()
  "Either create a tab or show completion candidates.

If the tab name doesn't exist, then create a tab with the given
name and switch to it. If there's only one other tab, then
automatically switch to it."
  (interactive)
  (let ((tabs (mapcar (lambda (tab)
                        (alist-get 'name tab))
                      (tab-bar--tabs-recent))))
    (cond
     ((eq tabs nil)
      (tab-bar-new-tab))
     ((eq (length tabs) 1)
      (tab-bar-switch-to-next-tab))
     (t
      (consult--jump
       (pkg-tab-bar/select-tab-action
        (consult--read "Select tab: "
                       tabs
                       :sort nil
                       :require-match t
                       :category 'tab)))))))

(defun pkg-tab-bar/open-project-as-tab (project)
  (interactive (list (project-prompt-project-dir)))
  (tab-bar-new-tab)
  (project-switch-project project)
  (tab-bar-rename-tab (project-name (project-current))))

(setq tab-bar-close-button-show nil
      tab-bar-close-last-tab-choice nil ; Do nothing and show a message.
      tab-bar-close-tab-select 'recent
      tab-bar-new-tab-to 'right
      tab-bar-new-button-show nil
      tab-bar-new-tab-choice t      ; Start a new tab with the current buffer.
      tab-bar-position nil          ; Show above the tool bar.
      tab-bar-show 1                ; Show only when there's more than 1 tab.
      tab-bar-tab-hints nil         ; Do not show numbers on tabs.
      tab-bar-tab-name-function #'tab-bar-tab-name-all)

(tab-bar-history-mode -1)

;; Use the "C-x t" prefix in order to be consistent with standard Emacs
;; keybindings.
(define-key my/keys-mode-map (kbd "C-x t t") #'pkg-tab-bar/select-tab-dwim)

(provide 'pkg-tab-bar)
