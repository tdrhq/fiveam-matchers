(defpackage :fiveam-matchers/test-errors
  (:use #:cl
        #:fiveam)
  (:import-from #:fiveam-matchers/core
                #:matchesp)
  (:import-from #:fiveam-matchers/errors
                #:signals-error-matching
                #:error-with-string-matching)
  (:import-from #:fiveam-matchers/strings
                #:matches-regex))
(in-package :fiveam-matchers/test-errors)

(def-suite* :fiveam-matchers/test-errors)


(test signals-error-matching
  (signals-error-matching ()
   (error "this is a test")
   (error-with-string-matching
    (matches-regex "this.*test"))))

(test error-matching-with-regex
  (signals-error-matching ()
   (error "this is a test")
   "this.*test"))

(test signals-check-failure
  (is-true
   (matchesp (error-with-string-matching (matches-regex "foo"))
             (make-condition 'simple-error :format-control "foo")))
  (is-true
   (matchesp (error-with-string-matching "foo")
             (make-condition 'simple-error :format-control "foo")))
  (is-false
   (matchesp (error-with-string-matching "bar")
             (make-condition 'simple-error :format-control "foo"))))
