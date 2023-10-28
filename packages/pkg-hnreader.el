;;; -*- lexical-binding: t; -*-

(lib-util/pkg hnreader
  :elpaca (:ref "8444e177035e236e991f9ea73074c053a45426ad")
  :defer t
  :commands (hnreader-news
             hnreader-newest
             hnreader-ask
             hnreader-back
             hnreader-jobs
             hnreader-past))

(provide 'pkg-hnreader)
