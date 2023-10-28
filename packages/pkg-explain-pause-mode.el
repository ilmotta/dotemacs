;;; -*- lexical-binding: t; -*-

(require 'lib-util)

(lib-util/pkg explain-pause-mode
  :elpaca (:host github
           :repo "lastquestion/explain-pause-mode"
           :ref "2356c8c3639cbeeb9751744dbe737267849b4b51")
  :defer t)

(provide 'pkg-explain-pause-mode)
