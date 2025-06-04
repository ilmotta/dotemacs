;;; -*- lexical-binding: t; -*-

(require 'lib-util)

(lib-util/pkg protobuf-mode
  :ensure (:ref "44ac12471381a0331735c253b2d954fad3f8207c")
  :defer t)

(provide 'pkg-protobuf-mode)
