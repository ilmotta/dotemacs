;;; -*- lexical-binding: t; -*-
(require 'use-package)

;; A company front-end that uses child frames.
;;
;; ISSUE 154 https://github.com/sebastiencs/company-box/issues/154
;;
;; Disabled because of incomplete pop-up at the very first time of completion.
(lib-util/pkg company-box
  :straight t
  :disabled t
  :defer t
  :hook (company-mode-hook . company-box-mode)
  :config
  (setq company-box-enable-icon nil)
  (setq company-box-doc-enable nil)
  (setq company-box-scrollbar nil)
  (setq company-box-show-single-candidate 'always)
  ;; Follow cursor position.
  (setq company-box-frame-behavior 'point))

(provide 'pkg-company-box)
