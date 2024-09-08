;;; -*- lexical-binding: t; -*-

;;; Commentary:
;; The main advantages of Jinx are its automatic checking of the visible text,
;; its sharp focus on performance and the ability to easily use multiple
;; dictionaries at once.
;;
;; Flyspell highlights misspellings while typing. Only the word under the cursor
;; is spell-checked. Jinx, on the other hand, is more effective because it
;; automatically checks for misspellings in the entire visible text of the
;; buffer at once. Flyspell can check the entire buffer but must be instructed
;; to do so via the command flyspell-buffer.

;;; Code:

(require 'lib-util)

(lib-util/pkg jinx
  :elpaca nil
  :defer t
  :init
  (general-def
    :keymaps 'my/keys-mode-map
    [remap ispell-word] #'jinx-correct))

(provide 'pkg-jinx)
