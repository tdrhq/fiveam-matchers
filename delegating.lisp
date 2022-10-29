(defpackage :fiveam-matchers/delegating
  (:use #:cl)
  (:import-from #:fiveam-matchers/core
                #:describe-mismatch
                #:describe-self
                #:matchesp
                #:matcher)
  (:local-nicknames (#:a #:alexandria)))
(in-package :fiveam-matchers/delegating)

(defclass delegating-matcher (matcher)
  ((matcher :initarg :matcher
            :reader delegate)
   (describe-self :initarg :describe-self
                  :initform nil)
   (describe-mismatch :initarg :describe-mismatch
                      :initform nil))
  (:documentation "Sometimes it's easier to create a matcher in terms of other matchers,
 but even in those situations you might want to modify the
 describe-self and describe-mismatch"))

(defmethod matchesp ((matcher delegating-matcher) actual)
  (matchesp (delegate matcher) actual))

(defmethod describe-self ((matcher delegating-matcher))
  (with-slots (describe-self) matcher
    (cond
      (describe-self
       (funcall describe-self))
      (t
       (describe-self (delegate matcher))))))

(defmethod describe-mismatch ((matcher delegating-matcher) actual)
  (with-slots (describe-mismatch) matcher
    (cond
      (describe-mismatch
       (funcall describe-mismatch actual))
      (t
       (describe-mismatch (delegate matcher) actual)))))
