;;; -*- lexical-binding: t; -*-

(lib-util/pkg gotest
  :elpaca (:ref "2ec82dcc70d5f6aa22f66b44f8b537be33bd7903")
  :defer t
  :init
  (my/general-mode-def
    :keymaps 'go-mode-map
    ;; Tests
    "t ." #'go-test-current-test
    "t b" #'go-test-current-file
    "t l" #'go-test-current-test-cache
    "t p" #'go-test-current-project))

(provide 'pkg-gotest)
