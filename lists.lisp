(uiop:define-package :fiveam-matchers/lists
    (:use #:cl
          #:alexandria)
  (:import-from #:fiveam-matchers/core
                #:is-not
                #:ensure-matcher
                #:self-describing-list
                #:describe-self
                #:esc
                #:describe-mismatch
                #:matchesp
                #:matcher)
  (:import-from #:fiveam-matchers/every-item
                #:every-item)
  (:import-from #:fiveam-matchers/delegating
                #:delegating-matcher)
  (:export
   #:contains
   #:has-item
   #:does-not-have-item))
(in-package :fiveam-matchers/lists)

(defclass contains (matcher)
  ((expected :initarg :expected
             :accessor expected)))

(defun contains (&rest expected)
  (let ((expected (mapcar 'ensure-matcher expected)))
   (make-instance 'contains
                  :expected expected)))

(defmethod matchesp ((matcher contains) (actual list))
  (when (eql (length actual)
             (length (expected matcher)))
    (not
     (loop for a in actual
           for x in (expected matcher)
           unless (matchesp x a)
             return t))))

(defmethod describe-self ((matcher contains))
  `("a list with values: "
    ,(self-describing-list
      (expected matcher))))

(defclass has-item (matcher)
  ((expected :initarg :expected
             :accessor expected)))

(defun has-item (expected)
  (let ((expected (ensure-matcher expected)))
    (make-instance 'has-item :expected expected)))

(defun does-not-have-item (expected)
  (let ((expected (ensure-matcher expected)))
    (make-instance
     'delegating-matcher
     :matcher (every-item (is-not expected))
     :describe-self (lambda ()
                      `("a sequence that does not contain: " ,(describe-self expected)))
     :describe-mismatch (lambda (actual)
                          (loop for item in actual
                                for idx from 0
                                if (matchesp expected item)
                                  return `("The item at idx " ,idx " matches"))))))

(defmethod matchesp ((matcher has-item) (actual list))
  (some (lambda (x)
         (matchesp (expected matcher) x))
        actual))

(defmethod describe-self ((matcher has-item))
  `("a sequence that contains: " ,(describe-self (expected matcher)) ))

(defmethod describe-mismatch ((matcher has-item) (actual list))
  `("none of the elements matched"))

(defmethod describe-mismatch ((matcher contains) (actual list))
  (cond
    ((not (eql (length actual)
               (length (expected matcher))))
     `("was a list not of length " ,(length (expected matcher)) " (actual length: "
                                   ,(length actual) ")"))
    (t
     (loop for a in actual
           for x in (expected matcher)
           for i from 0
           unless (matchesp x a)
             return `("The element at position "
                      ,(esc i)
                      " "
                      ,(describe-mismatch x a))))))
