;;;; Copyright 2018-Present Modern Interpreters Inc.
;;;;
;;;; This Source Code Form is subject to the terms of the Mozilla Public
;;;; License, v. 2.0. If a copy of the MPL was not distributed with this
;;;; file, You can obtain one at https://mozilla.org/MPL/2.0/.

(defpackage :fiveam-matchers/errors
  (:use #:cl
        #:fiveam-matchers/core)
  (:import-from #:fiveam-matchers/core
                #:matcher)
  (:import-from #:fiveam-matchers/strings
                #:matches-regex)
  (:export
   #:signals-error-matching
   #:error-with-string-matching))
(in-package :fiveam-matchers/errors)

(defclass error-with-string-matching (matcher)
  ((delegate :initarg :delegate
             :reader delegate)))

(defmethod matchesp ((matcher error-with-string-matching) value)
  (and
   (typep value 'error)
   (matchesp
    (delegate matcher)
    (princ-to-string value))))

(defmethod describe-mismatch-to-string ((matcher error-with-string-matching) value)
  (cond
    ((not (typep value 'error))
     (format nil "Wasn't provided an error type"))
    (t
     (format nil "Expected error with message to match: ~a"
             (describe-mismatch-to-string (delegate matcher) (princ-to-string value))))))

(defmethod describe-self ((matcher error-with-string-matching))
  (format nil "An error with message: ~a"
          (describe-self (delegate matcher))))

(defmethod error-with-string-matching (matcher)
  (make-instance 'error-with-string-matching :delegate matcher))

(defmethod error-with-string-matching ((matcher string))
  (error-with-string-matching (equal-to matcher)))

(defmacro signals-error-matching ((&optional (error-class 'simple-error)) expr &rest matchers)
  "Checks if a the expr signaled an error whose string representation
matches the given matchers. Useful for asserting SIMPLE-ERRORs.

If a matcher is just a string, it is treated as equivalent to a regex, i.e. (error-with-string-matching (matches-regex ...))."
  `(progn
     (let ((no-error nil))
      (handler-case
          (progn
            ,expr
            (setf no-error t))
        (,error-class (e)
          (assert-that
           e
           ,@ (loop for matcher in matchers
                    if (stringp matcher)
                      collect `(error-with-string-matching (matches-regex ,matcher))
                    else
                      collect matcher))))
       (if no-error
           (fiveam:fail "Expected to see an exception when running ~a, but didn't see any" ',expr)))))

