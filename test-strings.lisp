(defpackage :fiveam-matchers/test-strings
  (:use #:cl
        #:fiveam)
  (:import-from #:fiveam-matchers/strings
                #:matches-regex)
  (:import-from #:fiveam-matchers/core
                #:describe-mismatch
                #:format-description
                #:describe-self
                #:is-equal-to
                #:is-not
                #:assert-that))
(in-package :fiveam-matchers/test-strings)

(def-suite* :fiveam-matchers/test-strings)

(test string-matches-regex
  (let ((matcher (matches-regex ".*foo1+bar.*")))
    (assert-that "sfdsffoo11111barsdfdfd"
                 matcher)
    (assert-that "sdfdsffoobarsdfds"
                 (is-not matcher))
    (assert-that
     (with-output-to-string (s)
      (format-description (describe-self matcher) s))
     (is-equal-to "Matches regex `.*foo1+bar.*'"))
    (assert-that
     (with-output-to-string (s)
       (format-description (describe-mismatch matcher "foobar") s))
     (is-equal-to "`foobar' did not match `.*foo1+bar.*'"))))
