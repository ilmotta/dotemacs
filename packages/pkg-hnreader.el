;;; -*- lexical-binding: t; -*-

(require 'lib-util)

(lib-util/pkg hnreader
  :ensure (:ref "b422628a272d7672cc2c7dfcef1c8bf06371afd5")
  :defer t
  :commands (hnreader-news
             hnreader-newest
             hnreader-ask
             hnreader-back
             hnreader-jobs
             hnreader-past))

(provide 'pkg-hnreader)
