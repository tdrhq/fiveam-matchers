(defpackage :fiveam-matchers/satisfying
  (:use #:cl)
  (:import-from #:fiveam-matchers/core
                #:describe-mismatch
                #:describe-self
                #:matchesp
                #:matcher)
  (:local-nicknames (#:a #:alexandria)))
(in-package :fiveam-matchers/satisfying)

(defclass satisfying (matcher)
  ((expr :initarg :expr
         :reader expr)
   (predicate :initarg :predicate
              :reader predicate)))

(defmacro satisfying (expr)
  `(make-instance 'satisfying
                  :expr ',expr
                  :predicate (lambda (*)
                               ,expr)))

(defun expr-str (self)
  (prin1-to-string (expr self)))

(defmethod matchesp ((self satisfying) val)
  (funcall (predicate self) val))

(defmethod describe-self ((self satisfying))
  `("satisfies " ,(expr-str self)))

(defmethod describe-mismatch ((self satisfying) value)
  `(,(expr-str self) " was false for " ,value))
