;;; -*- lexical-binding: t; -*-

;;; Code:

(require 'lib-util)

(lib-util/pkg ement
  :elpaca (:repo "alphapapa/ement.el" :ref "1a74446c9396ef305f11c6cb07b94688a55577d9")
  ;; 2023-05-20: I haven't used it for a long time.
  ;; :disabled t
  :defer t)

(provide 'pkg-ement)
