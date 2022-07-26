;;; -*- lexical-binding: t; -*-

(my/package flyspell-correct
  :straight t
  :defer t

  :init
  (general-def
    ;; Evil binds this to z-=
    [remap ispell-word] #'flyspell-correct-wrapper)

  ;; Do not emit messages when checking words.
  (setq flyspell-issue-message-flag nil)

  ;; Do not display a welcome message when flyspell starts.
  (setq flyspell-issue-welcome-flag nil))

(provide 'pkg-flyspell-correct)
