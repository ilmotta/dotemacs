;;; -*- lexical-binding: t; -*-

(my/package eldoc
  :elpaca nil
  :init
  (setq eldoc-idle-delay 0.25
        eldoc-minor-mode-string nil
        eldoc-echo-area-use-multiline-p nil))

(provide 'pkg-eldoc)