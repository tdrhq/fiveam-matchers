(uiop:define-package :fiveam-matchers/test-core
    (:use #:cl
          #:fiveam
          #:alexandria)
  (:import-from #:fiveam-matchers/core
                #:error-with-string-matching
                #:signals-error-matching
                #:has-any
                #:has-all
                #:format-description
                #:assert-that
                #:is-not
                #:describe-mismatch
                #:matchesp
                #:equal-to)
  (:import-from #:fiveam-matchers/strings
                #:matches-regex))
(in-package #:fiveam-matchers/test-core)

(def-suite* :fiveam-matchers/test-core)

(test preconditions
  (pass))


(defun fmt-desc (desc)
  (with-output-to-string (s)
    (format-description
     desc s)))

(test equal-to-matcher
  (let ((matcher (equal-to "foobar")))
    (is-false (matchesp matcher "zoidberg"))
    (is-true (matchesp matcher "foobar"))
    (is (equal "was \"zoidberg\""
               (fmt-desc
                (describe-mismatch matcher "zoidberg"))))))

(test is-not-matcher
  (let ((matcher (is-not (equal-to "foobar"))))
    (is-true (matchesp matcher "zoidberg"))
    (is-false (matchesp matcher "foobar"))
    (is (equal "not was \"foobar\""
               (fmt-desc
                (describe-mismatch matcher "foobar"))))))

(test has-all
  (let ((matcher (has-all (equal-to "foobar")
                          (is-not (equal-to "zoidberg")))))
    (is-true (matchesp matcher "foobar"))
    (is-false (matchesp matcher "zoidberg"))
    (is-false (matchesp matcher "arnold"))))

(test has-any
  (let ((matcher (has-any (equal-to "foobar")
                          (is-not (equal-to "zoidberg")))))
    (is-true (matchesp matcher "foobar"))
    (is-false (matchesp matcher "zoidberg"))
    (is-true (matchesp matcher "arnold"))))

(test assert-that
  (assert-that "foobar"
               (equal-to "foobar")
               (is-not (equal-to "zoik")))
  ;; There's probably a way to test this next line, but it's not worth
  ;; the trouble for right now
  #+nil
  (assert-that "foobar3"
               (equal-to "foobar")))

(test debugger-works
  (let ((fiveam::*on-failure* :debug))
    (signals fiveam::check-failure
      (assert-that "foobar"
                   (equal-to "zoidberg")))))

(test signals-error-matching
  (signals-error-matching ()
   (error "this is a test")
   (error-with-string-matching
    (matches-regex "this.*test"))))

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
