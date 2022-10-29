(defpackage :fiveam-matchers/misc
  (:use #:cl)
  (:import-from #:fiveam-matchers/core
                #:is-not
                #:equal-to)
  (:import-from #:fiveam-matchers/described-as
                #:described-as)
  (:local-nicknames (#:a #:alexandria))
  (:export
   #:is-null))
(in-package :fiveam-matchers/misc)

(defun is-not-null ()
  (described-as "not to be null"
    (is-not (equal-to nil))))

(defun is-null ()
  (described-as "is null"
    (equal-to nil)))
