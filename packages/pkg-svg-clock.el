;;; -*- lexical-binding: t; -*-

;;; Commentary:
;; `svg-clock' provides a scalable analog clock.

;;; Code:

(require 'lib-util)

(lib-util/pkg svg-clock
  :elpaca (:ref "0b92fed41aa65238ae7f9716c59cdec583463933")
  :defer t)

(provide 'pkg-svg-clock)
