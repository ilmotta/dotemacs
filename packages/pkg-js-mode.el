;;; -*- lexical-binding: t; -*-

(defun pkg-js-mode/setup-sibling-rules ()
  (setq-local find-sibling-rules
              (list
               ;; Source -> unit test
               (list (rx (group (+ (not "/")))
                         "." (group (or "js" "jsx" "ts" "tsx"))
                         string-end)
                     (rx (regex "\\1")
                         ".test." (regex "\\2")
                         string-end))

               ;; Unit test -> source
               (list (rx (group (+ (not "/")))
                         ".test." (group (or "js" "jsx" "ts" "tsx"))
                         string-end)
                     (rx (regex "\\1")
                         "." (regex "\\2")
                         string-end))

               ;; Source -> spec test
               (list (rx (group (+ (not "/")))
                         "." (group (or "js" "jsx" "ts" "tsx"))
                         string-end)
                     (rx (regex "\\1")
                         ".spec." (regex "\\2")
                         string-end))

               ;; Spec test -> source
               (list (rx (group (+ (not "/")))
                         ".spec." (group (or "js" "jsx" "ts" "tsx"))
                         string-end)
                     (rx (regex "\\1")
                         "." (regex "\\2")
                         string-end)))))

(my/package js-mode
  :straight (:type built-in)
  :no-require t
  :defer t
  :hook ((typescript-tsx-mode-hook
          js-mode-hook) . pkg-js-mode/setup-sibling-rules))

(provide 'pkg-js-mode)
