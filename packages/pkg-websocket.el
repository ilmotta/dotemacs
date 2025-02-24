;;; -*- lexical-binding: t; -*-

;;; Code:

(require 'lib-util)

(lib-util/pkg websocket
  :elpaca (:ref "40c208eaab99999d7c1e4bea883648da24c03be3")
  :defer t)

(provide 'pkg-websocket)
