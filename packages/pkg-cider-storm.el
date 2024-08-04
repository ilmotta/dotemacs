;;; -*- lexical-binding: t; -*-

;;; Commentary:
;; Front-end for the FlowStorm debugger with support for Clojure(Script).
;;
;; Source: https://github.com/jpmonettas/cider-storm

;;; Code:

(require 'lib-util)

(lib-util/pkg cider-storm
  :elpaca (:host github
           :repo "jpmonettas/cider-storm"
           :ref "58b8a50406e5e6fbe2d8cdc969543246987386cd")
  :defer t)

(provide 'pkg-cider-storm)
