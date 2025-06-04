;;; -*- lexical-binding: t; -*-

(require 'lib-util)

(lib-util/pkg csv-mode
  :ensure (:host github
           :repo "emacsmirror/csv-mode"
           :ref "849ce3e754f291c3643bc36ed802226606955c3f"
           :branch nil)
  :defer t)

(provide 'pkg-csv-mode)
