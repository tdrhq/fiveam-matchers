;;;; Copyright 2018-Present Modern Interpreters Inc.
;;;;
;;;; This Source Code Form is subject to the terms of the Mozilla Public
;;;; License, v. 2.0. If a copy of the MPL was not distributed with this
;;;; file, You can obtain one at https://mozilla.org/MPL/2.0/.

(defpackage :fiveam-matchers/markup/test-tags
  (:use #:cl
        #:fiveam)
  (:import-from #:fiveam-matchers/core
                #:does-not
                #:assert-that
                #:is-equal-to)
  (:import-from #:fiveam-matchers/markup
                #:has-matching-descendent)
  (:import-from #:fiveam-matchers/markup/tags
                #:have-matching-descendent))
(in-package :fiveam-matchers/markup/test-tags)

(named-readtables:in-readtable markup:syntax)

(def-suite* :fiveam-matchers/markup/test-tags)

(def-fixture state ()
  (let ((code
          <div class= "foo">
            <form>
              <input type= "hidden" />
              <div>car</div>

              <markup:merge-tag>
                <span name= "foo">bleh</span>
              </markup:merge-tag>
            </form>
          </div>))
    (&body)))

(test with-matching-descendent
  (with-fixture state ()
    (assert-that
     code
     (has-matching-descendent (is-equal-to "car")))))

(test does-not-have-a-matching-desc
  (with-fixture state ()
    (assert-that
     code
     (does-not (have-matching-descendent (is-equal-to "far"))))))

(test looks-up-merge-tags
  (with-fixture state ()
    (assert-that
     code
     (has-matching-descendent (is-equal-to "bleh")))))
