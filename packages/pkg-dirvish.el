;;; -*- lexical-binding: t; -*-

(eval-and-compile
  (defun pkg-dirvish/load-paths ()
    (list (concat (straight--repos-dir "dirvish") (file-name-as-directory "extensions")))))

;; Dirvish is a minimalistic file manager based on dired.
;; Dirvish does not introduce any keybindings by default, see dirvish-mode-map for more details.
(my/package dirvish
  :straight (:host github :repo "alexluigit/dirvish")
  :load-path (lambda () (pkg-dirvish/load-paths))
  :defer t

  :init
  (setq dirvish-body-fontsize-increment 0.0)
  (setq dirvish-cache-dir (concat my/cache-dir "dirvish/"))
  (setq dirvish-enable-preview t)
  (setq dirvish-preview-delay 0.02)
  (setq dirvish-use-large-header nil)
  (setq dirvish-preview-cmd-alist '(("text/"
                                     (find-file-noselect t nil))
                                    ("image/"
                                     ("convert" "-resize" "%s" "%i" "%T"))
                                    ("audio/"
                                     ("mediainfo" "%i"))
                                    ("video/"
                                     ("ffmpegthumbnailer" "-i" "%i" "-o" "%T" "-s 0"))
                                    (("iso" "bin" "exe" "gpg")
                                     ("*Preview Disable*"))
                                    (("zip")
                                     ("zipinfo" "%i"))
                                    (("zst" "tar")
                                     ("tar" "-tvf" "%i"))
                                    (("epub")
                                     ("epub-thumbnailer" "%i" "%T" "1024"))
                                    (("pdf")
                                     (find-file-noselect
                                      '(t nil)))))

  ;; For selectrum or vertico users (only support these 2 completion UIs for
  ;; now), if you’d like to give this extension a try, all you need is:
  ;; (require 'dirvish-minibuffer-preview)
  ;; (dirvish-minibuf-preview-mode)
  :config

  (dirvish-override-dired-mode))

(provide 'pkg-dirvish)
