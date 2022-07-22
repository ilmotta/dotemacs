;;; -*- lexical-binding: t; -*-
;; Copyright (C) 2022 Icaro Motta

;; Author: Icaro Motta <icaro.ldm@gmail.com>
;; URL: http://github.com/ilmotta/dotfiles
;; Version: 1.0.0

;;; Commentary:
;;
;; TMSU integration layer.
;;
;; References
;; - https://github.com/oniony/TMSU
;; - https://github.com/oniony/TMSU/wiki
;;
;; tmsu mkdir /tmp/tmsu
;; tmsu mount /tmp/tmsu --database $HOME/.tmsu/db
;;
;; tmsu tag --tags="work repo" ~/data/repos/<repo>
;;
;; Ideas:
;;
;; - Tag marked items in Dired.
;; - Tag current file/dir in Dired.
;; - Display TMSU tags in Dired.
;; - Filter files and tags using `completing-read'.
;; - Files/tags navigation in tabulated mode.
;; - Manage files/tags in tabulated mode.
;; - In tabulated mode, paginate results.
;; - Create TMSU queries.
;; - Help user do filesystem operations, like rename.
;;

;;; Code:
(require 'consult)

(defvar t/-files-history nil)

(defgroup lib-tmsu nil
  "TMSU integration layer."
  :group 'my
  :prefix "t/")

(defcustom t/buffer "*tmsu*"
  "Name of the buffer to append tmsu output."
  :type 'string)

(defcustom t/database-dir
  (file-truename "~")
  "Directory where the TMSU database is stored."
  :type 'string)

;;;###autoload
(defun t/init ()
  (interactive)
  (let ((default-directory t/database-dir))
    (start-process-shell-command "tmsu" t/buffer "tmsu init")))

;;;###autoload
(defun t/files ()
  (interactive)
  (let ((initial "")
        (cmd "tmsu files ARG OPTS"))
    (consult--read
     (consult--async-command cmd)
     :prompt "Query: "
     :history '(:input t/-files-history)
     :state (lambda (cand _restore)
              (find-file cand))
     :initial (concat consult-async-default-split initial)
     :add-history (concat consult-async-default-split (thing-at-point 'symbol))
     :require-match t
     :sort nil)))

(provide 'lib-tmsu)

;; Local Variables:
;; read-symbol-shorthands: (("t/" . "lib-tmsu/"))
;; End:
