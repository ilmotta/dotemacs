;;; -*- lexical-binding: t; -*-

;;; Code:

(require 'lib-util)

(lib-util/pkg all-the-icons
  :ensure (:ref "51bf77da1ebc3c199dfc11f54c0dce67559f5f40")
  :when (display-graphic-p)
  :defer t
  :commands (all-the-icons-octicon
             all-the-icons-faicon
             all-the-icons-fileicon
             all-the-icons-wicon
             all-the-icons-material
             all-the-icons-alltheicon)

  :init
  ;; IMPORTANT: changing the variables below may require restarting Emacs.

  ;; This is an important fix when using posframe, because certain lines at the
  ;; end would not be displayed because I prefer to set posframes with fixed
  ;; heights. Anyway, I actually prefer smaller icons.
  (setq all-the-icons-scale-factor 1.0)

  ;; Display colored icons in dired buffers.
  (setq all-the-icons-dired-monochrome nil)

  :config
  (with-eval-after-load 'org-capture
    (setq org-capture-templates
          (append
           `(("n" ,(concat (all-the-icons-faicon "sticky-note" :v-adjust -0.1) " Note")
              plain
              (file pkg-org-capture/note-new-path)
              ,pkg-org-capture/template-note
              :immediate-finish t
              :jump-to-captured t
              :no-save t))
           `(("t" ,(concat (all-the-icons-faicon "tasks") " Task")
              entry (function
                     (lambda ()
                       (pkg-org-capture/first-headline "~/data/repos/notes/20240625094859.org")))
              ,pkg-org-capture/template-todo
              :immediate-finish t
              :prepend t)))))

  ;; Icons by file name.
  (add-to-list 'all-the-icons-icon-alist '("\\.ledger$" all-the-icons-faicon "money" :height 1.0 :v-adjust 0.0 :face all-the-icons-dgreen))
  (add-to-list 'all-the-icons-icon-alist '("\\.conf$" all-the-icons-octicon "settings" :height 1.0 :v-adjust 0.0 :face all-the-icons-dyellow))
  (add-to-list 'all-the-icons-icon-alist '("\\.service$" all-the-icons-octicon "settings" :height 1.0 :v-adjust 0.0 :face all-the-icons-dyellow))
  (add-to-list 'all-the-icons-icon-alist '("\\.timer$" all-the-icons-octicon "settings" :height 1.0 :v-adjust 0.0 :face all-the-icons-dyellow))
  (add-to-list 'all-the-icons-icon-alist '("^config$" all-the-icons-octicon "settings" :height 1.0 :v-adjust 0.0 :face all-the-icons-dyellow))

  ;; Icons by mode.
  (add-to-list 'all-the-icons-mode-icon-alist '(exwm-mode all-the-icons-faicon "cube" :v-adjust -0.1 :face all-the-icons-red))

  ;; Icons by directory name.
  (add-to-list 'all-the-icons-dir-icon-alist '("emacs" all-the-icons-fileicon "emacs"))
  (add-to-list 'all-the-icons-dir-icon-alist '("emacs\\.d" all-the-icons-fileicon "emacs")))

(provide 'pkg-all-the-icons)
