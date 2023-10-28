;;; -*- lexical-binding: t; -*-

;;; Commentary:
;; Easily open file, dired buffer or magit buffer in its respective remote host.

;;; Code:

(lib-util/pkg browse-at-remote
  :elpaca (:ref "d81643c975e77d506fe2eb931229739c162adb5d")
  :defer t
  :init
  (general-def
    :keymaps 'pkg-magit/command-map
    "r" #'browse-at-remote)

  ;; Use the commit hash. Unfortunately when set to non-nil the package
  ;; sometimes fails.
  (setq browse-at-remote-prefer-symbolic nil)

  :config
  (setq browse-at-remote-remote-type-regexps
        `((:host ,(rx bol "ilmotta-github" eol)
           :type "github"
           :actual-host "github.com")
          (:host ,(rx bol "github.com" eol)
           :type "github")
          (:host ,(rx bol "bitbucket.org" eol)
           :type "bitbucket")
          (:host ,(rx bol "gitlab.com" eol)
           :type "gitlab")
          (:host ,(rx bol "git.savannah.gnu.org" eol)
           :type "gnu")
          (:host ,(rx bol "gist.github.com" eol)
           :type "gist")
          (:host ,(rx bol "git.sr.ht" eol)
           :type "sourcehut")
          (:host ,(rx bol "gitlab.gnome.org" eol)
           :type "gitlab"))))

(provide 'pkg-browse-at-remote)
