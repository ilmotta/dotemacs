;;; -*- lexical-binding: t; -*-

(lib-util/pkg winner
  :elpaca nil
  :init
  (setq winner-dont-bind-my-keys t)
  (setq winner-boring-buffers
        '("*Apropos*"
          "*Buffer List*"
          "*Compile-Log*"
          "*Fuzzy Completions*"
          "*Help*"
          "*Ibuffer*"
          "*cvs*"
          "*esh command on file*"
          "*inferior-lisp*"))
  :config
  (winner-mode +1))

(provide 'pkg-winner)
