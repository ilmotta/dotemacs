;;; -*- lexical-binding: t; -*-

;;; Commentary:
;; https://github.com/oantolin/orderless provides an orderless completion style
;; that divides the pattern into space-separated components, and matches
;; candidates that match all of the components in any order. Each component can
;; match in any one of several ways: literally, as a regexp, as an initialism,
;; in the flex style, or as multiple word prefixes. By default, regexp and
;; initialism matches are enabled.

;;; Code:

(defun pkg-orderless/dispatcher (pattern _index _total)
  (cond
   ;; Ensure $ works with Consult commands, which add disambiguation suffixes
   ((string-suffix-p "$" pattern)
    `(orderless-regexp . ,(concat (substring pattern 0 -1) "[\x100000-\x10FFFD]*$")))
   ;; Ignore single !
   ((string= "!" pattern) `(orderless-literal . ""))
   ;; Without literal
   ((string-prefix-p "!" pattern) `(orderless-without-literal . ,(substring pattern 1)))
   ;; Initialism matching
   ((string-prefix-p "`" pattern) `(orderless-initialism . ,(substring pattern 1)))
   ((string-suffix-p "`" pattern) `(orderless-initialism . ,(substring pattern 0 -1)))
   ;; Literal matching
   ((string-prefix-p "=" pattern) `(orderless-literal . ,(substring pattern 1)))
   ((string-suffix-p "=" pattern) `(orderless-literal . ,(substring pattern 0 -1)))
   ;; Flex matching
   ((string-prefix-p "~" pattern) `(orderless-flex . ,(substring pattern 1)))
   ((string-suffix-p "~" pattern) `(orderless-flex . ,(substring pattern 0 -1)))))

(my/package orderless
  :straight t
  :defer t

  :init
  (general-def
    :keymaps 'minibuffer-local-completion-map
    ;; SPC should never complete: use it for `orderless' groups.
    my/leader nil)

  (setq completion-styles '(orderless))
  (setq completion-category-defaults nil)
  (setq orderless-style-dispatchers '(pkg-orderless/dispatcher))

  ;; Note that despite override in the name orderless can still be used in
  ;; find-file etc.
  (setq completion-category-overrides
        '((file (styles . (orderless partial-completion)))))

  ;; Available matching styles:
  ;;
  ;; '(orderless-regexp
  ;;   orderless-literal
  ;;   orderless-initialism
  ;;   orderless-strict-initialism
  ;;   orderless-strict-leading-initialism
  ;;   orderless-strict-full-initialism
  ;;   orderless-prefixes
  ;;   orderless-flex)
  ;;
  ;; Note: orderless-flex is quite slow.
  (setq orderless-matching-styles '(orderless-literal orderless-regexp))

  ;; A component separator like # allows multiple components in the company
  ;; candidates.
  (setq orderless-component-separator "[ &]")

  ;; This like the behavior of isearch when `search-upper-case' is non-nil.
  (setq orderless-smart-case t)

  :config
  ;; Otherwise find-file gets different highlighting than other commands.
  (set-face-attribute 'completions-first-difference nil :inherit nil))

(provide 'pkg-orderless)
