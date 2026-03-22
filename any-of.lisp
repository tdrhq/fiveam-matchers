;;;; Copyright 2018-Present Modern Interpreters Inc.
;;;;
;;;; This Source Code Form is subject to the terms of the Mozilla Public
;;;; License, v. 2.0. If a copy of the MPL was not distributed with this
;;;; file, You can obtain one at https://mozilla.org/MPL/2.0/.

(defpackage :fiveam-matchers/any-of
  (:use #:cl)
  (:import-from #:fiveam-matchers/core
                #:describe-self
                #:describe-mismatch
                #:matchesp
                #:matcher)
  (:export
   #:any-of))
(in-package :fiveam-matchers/any-of)

(defclass any-of (matcher)
  ((matchers :initarg :matchers
             :reader matchers)))

(defmethod any-of (&rest matchers)
  (make-instance 'any-of :matchers matchers))

(defmethod matchesp ((self any-of)
                     value)
  (loop for matcher in (matchers self)
        if (matchesp matcher value)
          return t))

(defmethod describe-self ((self any-of))
  `("Expected one of the following to match: "
    ,(loop for matcher in (matchers self)
           appending (list (describe-self matcher) "
"))))

(defmethod describe-mismatch ((self any-of) value)
  `("None matched. Here's why: "
    ,(loop for matcher in (matchers self)
           appending (list (describe-mismatch matcher value) "
"))))
