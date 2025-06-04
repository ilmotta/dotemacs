;;; -*- lexical-binding: t; -*-

;;; Code:

(defconst lib-metabase/port 8990)
(defconst lib-metabase/db-file (expand-file-name "~/.localib-metabase/var/metabase/metabase.db"))
(defconst lib-metabase/jar (expand-file-name "~/.localib-metabase/bin/metabase.jar"))
(defconst lib-metabase/plugins-dir (expand-file-name "~/.localib-metabase/var/metabase/plugins"))

;;;###autoload
(defun lib-metabase/start ()
  "Start the Metabase server."
  (interactive)
  (if (lib-utilib-metabase/listening-p lib-metabase/port)
      (message "Metabase is already running on port %s." lib-metabase/port)
    (let ((cmd (string-join
                (list
                 "export MB_CHECK_FOR_UPDATES=false"
                 (format "export MB_PLUGINS_DIR=\"%s\"" lib-metabase/plugins-dir)
                 (format "export MB_DB_FILE=\"%s\"" lib-metabase/db-file)
                 (format "export MB_JETTY_PORT=%s" lib-metabase/port)
                 (format "java -jar \"%s\"" lib-metabase/jar))
                "\n")))
      (start-process-shell-command "metabase-start" "*Metabase*" cmd))))

;;;###autoload
(defun lib-metabase/stop ()
  "Stop the Metabase server."
  (interactive)
  (when-let* ((pid (car (lib-utilib-metabase/process-pids "metabase.jar"))))
    (start-process-shell-command
     "metabase-kill" "*Metabase*" (concat "kill -9 " pid))))

;;;###autoload
(defun lib-metabase/backup ()
  "Backup the H2 Metabase file."
  (interactive)
  (if (lib-utilib-metabase/listening-p lib-metabase/port)
      (message "Error: Metabase is running.")
    (let ((cmd (format "tar -czvf \"%s\" \"%s\""
                       (expand-file-name "~/data/backups/metabase.db.mv.db.tar.gz")
                       (concat lib-metabase/db-file ".mv.db"))))
      (start-process-shell-command "metabase-backup" "*Metabase-Backup*" cmd))))

;;;###autoload
(defun lib-metabase/update ()
  "Update Metabase from *.ledger files."
  (interactive)
  (let ((tmp-file (make-temp-file "ledger-" nil ".csv")))
    (unwind-protect
        (progn
          ;; Dump all posts to a temporary CSV file.
          (with-temp-file tmp-file
            (let ((cmd (string-join
                        (list "cat ~/data/finance/*.ledger | ledger -f - register"
                              "--date-format '%Y-%m-%d'"
                              "--no-pager"
                              "--no-color"
                              (format  "-F '%s\n'"
                                       (string-join (list "%(date)"
                                                          "%(commodity)"
                                                          "%(quantity(amount))"
                                                          "%(account)"
                                                          "%(quoted(payee))")
                                                    ",")))
                        " ")))
              (call-process-shell-command cmd nil (buffer-name))))

          ;; Create SQLite database and import the CSV.
          (let ((cmd (concat "sqlite3 ~/.localib-metabase/var/metabase/finance.db <<EOF\n"
                             "DROP TABLE IF EXISTS posts;"
                             "CREATE TABLE posts ("
                             "  date DATE NOT NULL,"
                             "  commodity TEXT NOT NULL,"
                             "  quantity NUMERIC NOT NULL,"
                             "  account TEXT NOT NULL,"
                             "  payee TEXT NOT NULL"
                             ");\n"
                             ".mode csv\n"
                             ".import " tmp-file " posts\n"
                             "EOF")))
            (with-temp-buffer
              (call-process-shell-command cmd nil (buffer-name))
              (buffer-string))))

      ;; Make sure we remove spurious files.
      (delete-file tmp-file))))

(provide 'lib-metabase)
