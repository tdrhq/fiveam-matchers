(defpackage :fiveam-matchers/strings
  (:use #:cl
        #:fiveam-matchers/core)
  (:import-from #:fiveam-matchers/core
                #:is-not
                #:has-typep
                #:has-all)
  (:import-from #:fiveam-matchers/misc
                #:is-not-null)
  (:import-from #:fiveam-matchers/described-as
                #:described-as)
  (:local-nicknames (#:a #:alexandria))
  (:export
   #:starts-with
   #:contains-string
   #:matches-regex
   #:is-not-empty
   #:is-string))
(in-package :fiveam-matchers/strings)

(defclass starts-with-matcher (matcher)
  ((prefix :initarg :prefix
           :reader prefix)))

(defmethod starts-with ((prefix string))
  (make-instance 'starts-with-matcher
                  :prefix prefix))

(defmethod matchesp ((matcher starts-with-matcher)
                     actual)
  (and
   (stringp actual)
   (str:starts-with-p (prefix matcher)
                      actual)))

(defmethod describe-self ((matcher starts-with-matcher))
  `("a string that starts with `" ,(prefix matcher) "`"))

(defmethod describe-mismatch ((matcher starts-with-matcher) actual)
  `("expected `" ,actual "` to start with " ,(prefix matcher) ))


(defclass contains-string-matcher (matcher)
  ((needle :initarg :prefix
           :reader needle)))

(defmethod contains-string ((needle string))
  (make-instance 'contains-string-matcher
                  :prefix needle))

(defmethod matchesp ((matcher contains-string-matcher)
                     actual)
  (and
   (stringp actual)
   (str:containsp (needle matcher)
                  actual)))

(defmethod describe-self ((matcher contains-string-matcher))
  `("a string that contains `" ,(needle matcher) "`"))

(defmethod describe-mismatch ((matcher contains-string-matcher) actual)
  `("expected `" ,actual "` to contain " ,(needle matcher) ))


(defclass matches-regex-matcher (matcher)
  ((regex :initarg :regex
          :reader regex)))

(defun matches-regex (regex)
  (make-instance 'matches-regex-matcher :regex regex))

(defmethod matchesp ((self matches-regex-matcher)
                     actual)
  (cl-ppcre:scan (regex self) actual))

(defmethod describe-self ((matcher matches-regex-matcher))
  `("Matches regex `" ,(regex matcher) "'"))

(defmethod describe-mismatch ((matcher matches-regex-matcher)
                               actual)
  `("`" ,actual "' did not match `" ,(regex matcher) "'"))

(defun is-string ()
  (has-typep 'string))

(defun is-not-empty ()
  (described-as "Expected to be non-empty string"
   (has-all
    (is-string)
    (is-not-null)
    (is-not (equal-to "")))))
