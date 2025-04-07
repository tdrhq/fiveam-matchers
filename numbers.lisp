(defpackage :fiveam-matchers/numbers
  (:use #:cl)
  (:import-from #:fiveam-matchers/core
                #:describe-self
                #:describe-mismatch
                #:matchesp
                #:matcher))
(in-package :fiveam-matchers/numbers)


(defclass is-number-close-to (matcher)
  ((expected :initarg :expected
             :reader expected)
   (allowed-error :initarg :allowed-error
                  :reader allowed-error)))

(defun is-number-close-to (expected &key (allowed-error 0.0001))
  (make-instance 'is-number-close-to
                 :expected expected
                 :allowed-error allowed-error))

(defmethod matchesp ((self is-number-close-to)
                     value)
  (and
   (numberp value)
   (<
    (abs (- value (expected self)))
    (allowed-error self))))

(defmethod describe-mismatch ((self is-number-close-to)
                              value)
  `("Expected number to be close to: " ,(expected self)
                                       ", but was: "
                                       ,value))

(defmethod describe-self ((self is-number-close-to))
  `(("Expected a number close to " ,(expected self))))


