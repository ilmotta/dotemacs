;;; -*- lexical-binding: t; -*-

;;; Commentary:
;; Work with Git forges, such as Github and Gitlab, from the comfort of Magit
;; and the rest of Emacs.

;;; Code:

(require 'lib-util)

(lib-util/pkg forge
  ;; Disabled because I bumped over too much unexpected behavior.
  :disabled t

  :ensure (:host github
           :repo "magit/forge"
           :ref "0b5571b40e544bd182e5ed76d2400fe2fa8716d1")
  :defer t
  :init
  (setq forge-database-file (file-name-concat my/cache-dir "forge-database.sqlite"))
  (setq forge-alist '(("github.com" "api.github.com" "github.com" forge-github-repository)
                      ("ilmotta-github" "api.github.com" "ilmotta-github" forge-github-repository))))

(provide 'pkg-magit-forge)
