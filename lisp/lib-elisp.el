;;; -*- lexical-binding: t; -*-

;;; Commentary:
;; Code that largely helps write Elisp, unnamespaced for extra convenience.

;;; Code:

;;; Development macros
;; Macros that extend or facilitate development with Elisp. They are not
;; namespaced to feel more like native code.

(defmacro defhof (name fn-factory)
  `(progn
     (defvar ,name ,fn-factory)
     (fset ',name ,name)
     (declare-function ,name nil)))

(defmacro delq! (elt list &optional fetcher)
  "`delq' ELT from LIST in-place.

If FETCHER is a function, ELT is used as the key in LIST (an alist)."
  `(setq ,list
    (delq ,(if fetcher
               `(funcall ,fetcher ,elt ,list)
             elt)
     ,list)))

(defmacro pushnew! (place &rest values)
  "Push VALUES sequentially into PLACE, if they aren't already present.
This is a variadic `cl-pushnew'."
  (let ((var (make-symbol "result")))
    `(dolist (,var (list ,@values) (with-no-warnings ,place))
      (cl-pushnew ,var ,place :test #'equal))))

(defmacro comment (&rest _body)
  "Ignores body, yields nil."
  (declare (indent defun))
  nil)

(defmacro with-delay (delay &rest body)
  (declare (indent defun))
  `(run-with-timer ,delay nil (lambda () ,@body)))

(defmacro with-timer (delay repeat &rest body)
  (declare (indent defun))
  `(run-with-timer ,delay ,repeat (lambda () ,@body)))

;;; Advice

(defun advice-alist (symbol)
  "Returns an alist of (ADVICE-FUNCTION . ADVICE-PROPS) for SYMBOL.
See `advice-mapc'."
  (let ((fns nil))
    (advice-mapc (lambda (fn props)
                   (push `(,fn . ,props) fns))
                 symbol)
    fns))

(provide 'lib-elisp)
