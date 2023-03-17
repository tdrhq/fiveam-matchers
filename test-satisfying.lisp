(defpackage :fiveam-matchers/test-satisfying
  (:use #:cl
        #:fiveam)
  (:import-from #:fiveam-matchers/satisfying
                #:satisfying)
  (:import-from #:fiveam-matchers/core
                #:matchesp)
  (:local-nicknames (#:a #:alexandria)))
(in-package :fiveam-matchers/test-satisfying)

(def-suite* :fiveam-matchers/test-satisfying)

(test satisfying
  (is-true
   (matchesp
    (satisfying (evenp *))
    4))
  (is-false
   (matchesp
    (satisfying (evenp *))
    3))
  (is-true
   (matchesp
    (satisfying (evenp (+ 1 *)))
     3)))
