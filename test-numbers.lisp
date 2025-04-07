(defpackage :fiveam-matchers/test-numbers
  (:use #:cl
        #:fiveam)
  (:import-from #:fiveam-matchers/numbers
                #:is-number-close-to)
  (:import-from #:fiveam-matchers/core
                #:describe-self-to-string
                #:describe-mismatch-to-string
                #:describe-mismatch
                #:assert-that)
  (:import-from #:fiveam-matchers/strings
                #:matches-regex))
(in-package :fiveam-matchers/test-numbers)

(def-suite* :fiveam-matchers/test-numbers)

(test simple-matching
  (assert-that
   0.999999999
   (is-number-close-to 1.0)))

(test description-for-is-number-close-to
  (assert-that
   (describe-mismatch-to-string
    (is-number-close-to 1.0)
    2.0)
   (matches-regex ".*Expected number to be close to: 1.0, but was: 2.0.*")))

(test describe-self-for-is-number-close-to
  (assert-that
   (describe-self-to-string
    (is-number-close-to 1.0))
   (matches-regex "Expected a number close to 1.0")))
