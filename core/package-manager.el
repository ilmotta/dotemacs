;;; -*- lexical-binding: t; -*-

;;; Code:

(defvar elpaca-installer-version 0.10)
(defvar elpaca-directory (expand-file-name (file-name-concat my/local-dir "elpaca/")))
(defvar elpaca-builds-directory (file-name-concat elpaca-directory "builds/"))
(defvar elpaca-repos-directory (file-name-concat elpaca-directory "repos/"))

(defvar elpaca-order
  '(elpaca
       :repo "https://github.com/progfolio/elpaca.git"
       ;; Revision date: 2025-02-23
       :ref "b5ea4254ede6126f68363c7ba352ed4869c8517d"
       :files (:defaults "elpaca-test.el" (:exclude "extensions"))
       :build (:not elpaca--activate-package)))

(let* ((repo  (expand-file-name "elpaca/" elpaca-repos-directory))
       (build (expand-file-name "elpaca/" elpaca-builds-directory))
       (order (cdr elpaca-order))
       (default-directory repo))
  (add-to-list 'load-path (if (file-exists-p build) build repo))
  (unless (file-exists-p repo)
    (make-directory repo t)
    (when (<= emacs-major-version 28) (require 'subr-x))
    (condition-case-unless-debug err
        (if-let* ((buffer (pop-to-buffer-same-window "*elpaca-bootstrap*"))
                  ((zerop (apply #'call-process `("git" nil ,buffer t "clone"
                                                  ,@(when-let* ((depth (plist-get order :depth)))
                                                     (list (format "--depth=%d" depth) "--no-single-branch"))
                                                  ,(plist-get order :repo) ,repo))))
                  ((zerop (call-process "git" nil buffer t "checkout"
                                        (or (plist-get order :ref) "--"))))
                  (emacs (concat invocation-directory invocation-name))
                  ((zerop (call-process emacs nil buffer nil "-Q" "-L" "." "--batch"
                                        "--eval" "(byte-recompile-directory \".\" 0 'force)")))
                  ((require 'elpaca))
                  ((elpaca-generate-autoloads "elpaca" repo)))
            (progn (message "%s" (buffer-string)) (kill-buffer buffer))
          (error "%s" (with-current-buffer buffer (buffer-string))))
      ((error) (warn "%s" err) (delete-directory repo 'recursive))))
  (unless (require 'elpaca-autoloads nil t)
    (require 'elpaca)
    (elpaca-generate-autoloads "elpaca" repo)
    (load "./elpaca-autoloads")))
(add-hook 'after-init-hook #'elpaca-process-queues)
(elpaca `(,@elpaca-order))

;; I still need to verify if the lock file solution is reliable.
;;
;; (setq elpaca-lock-file (expand-file-name (file-name-concat user-emacs-directory "elpaca-lock.eld")))

(elpaca elpaca-use-package
  ;; Enable :elpaca use-package keyword.
  (elpaca-use-package-mode)
  ;; Assume :elpaca t unless otherwise specified.
  (setq elpaca-use-package-by-default t))

;; Block until current queue processed.
(elpaca-wait)

(require 'use-package)
(eval-and-compile
  (setq use-package-always-defer nil
        use-package-always-demand nil
        use-package-expand-minimally nil
        use-package-hook-name-suffix nil)

  ;; Compute statistics for `use-package' declarations. You can view the
  ;; statistical report using `use-package-report'.
  (setq use-package-compute-statistics nil))
