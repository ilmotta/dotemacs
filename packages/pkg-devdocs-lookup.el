;;; -*- lexical-binding: t; -*-

;;; Commentary:
;; Quickly jump to documentation on a particular API at devdocs.io with your
;; browser. It works similarly to javadoc-lookup, using your locally-configured
;; completing-read to select an entry.

;;; Code:

(defvar my/devdocs-lookup-loaded-p nil)

(defun my/devdocs-lookup-setup ()
  (require 'devdocs-lookup)
  (add-to-list 'devdocs-subjects '("Clojure 1.10" "clojure~1.10"))
  (setq devdocs-base-url "http://localhost:9292")
  (setq devdocs-base-index-url "http://localhost:9292/docs")
  (devdocs-setup))

(defun devdocs-lookup-clojure ()
  (interactive)
  (unless my/devdocs-lookup-loaded-p
    (my/devdocs-lookup-setup)
    (setq my/devdocs-lookup-loaded-p t))
  (devdocs-lookup-clojure~1.10))

(my/package devdocs-lookup
  :straight (:host github :repo "skeeto/devdocs-lookup")
  :defer t
  :commands (devdocs-lookup
             devdocs-lookup-clojure)
  :config
  (my/devdocs-lookup-setup))

(provide 'pkg-devdocs-lookup)
