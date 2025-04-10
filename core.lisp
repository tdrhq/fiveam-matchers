(uiop:define-package :fiveam-matchers/core
    (:use #:cl
          #:alexandria)
  (:export
   #:equal-to
   #:is-not
   #:matcher
   #:assert-that
   #:describe-self
   #:describe-mismatch
   #:single-value-matcher
   #:self-describing-list
   #:has-all
   #:has-any
   #:has-typep
   #:ensure-matcher
   #:matchesp
   #:is-equal-to
   #:signals-error-matching
   #:error-with-string-matching))
(in-package :fiveam-matchers/core)

(defclass matcher ()
  ())

(defun render-description (description output)
  (loop for x in description
        do
           (etypecase x
             (string
              (format output "~a" description))
             (list
              (render-description x output)))))

(defgeneric matchesp (matcher value))

(defgeneric describe-mismatch (matcher value))

(defmethod describe-self ((matcher matcher))
  `(" matches a matcher of type " ,(type-of matcher)))

;; core matchers

(defclass single-value-matcher (matcher)
  ((value :initarg :value
          :reader value)))

(defclass equal-to (single-value-matcher)
  ())

(defun equal-to (val)
  (make-instance 'equal-to :value val))

(defun is-equal-to (val)
  "Synonym for equal-to"
  (equal-to val))

(defun ensure-matcher (val)
  (typecase val
    (matcher val)
    (t (equal-to val))))

(defmethod matchesp ((matcher equal-to) value)
  (equal (value matcher) value))

(defmethod describe-mismatch ((matcher equal-to) value)
  `("was " ,(esc value)))

(defmethod describe-mismatch (matcher value)
  `("No description of mismatch available, see expression "))

(defmethod describe-self ((matcher equal-to))
  (esc (value matcher)))

(defclass is-not (matcher)
  ((value :initarg :value
          :reader value)))

(defun is-not (value)
  (check-type value matcher)
  (make-instance 'is-not :value value))

(defun does-not (value)
  ;; synonym
  (is-not value))

(defmethod matchesp ((matcher matcher) value)
  ;; This allows for specializing of matchesp on the value method
  nil)

(defmethod matchesp ((is-not is-not) value)
  (not (matchesp (value is-not) value)))

(defmethod describe-self ((is-not is-not))
  `("not " ,(describe-self (value is-not))))

(defmethod describe-mismatch ((is-not is-not) value)
  `("not " ,(describe-mismatch (value is-not) value)))

;; finally, an assert-that function

(defclass escaped ()
  ((value :initarg :value
          :reader value)))

(defun esc (x)
  (make-instance 'escaped :value x))

(defclass self-describing-list ()
  ((value :initarg :value
          :reader value)
   (start :initarg :start
          :initform "["
          :reader start)
   (end :initarg :end
        :initform "]"
        :reader end)
   (sep :initarg :sep
        :reader sep
        :initform ", ")))

(defun self-describing-list (list &rest args)
  (apply 'make-instance 'self-describing-list
                 :value list
                 args))

(defun format-description (description stream)
  (etypecase description
    (list
     (loop for x in description do
       (format-description x stream)))
    (escaped
     (format stream "~S" (value description)))
    (self-describing-list
     (format stream "~a" (start description))
     (loop for (x . next) on (value description)
           do
              (progn
                (format-description (describe-self x)
                                    stream)
                (when next
                  (format stream "~a" (sep description)))))
     (format stream "~a" (end description)))
    (t
     (format stream "~a" description))))

(defclass has-all (matcher)
  ((matchers :initarg :matchers
             :reader matchers)))

(defclass has-any (matcher)
  ((matchers :initarg :matchers
             :reader matchers)))

(defmethod matchesp ((has-all has-all) value)
  (not
   (loop for matcher in (matchers has-all)
         unless (matchesp matcher value)
           return t)))

(defmethod describe-mismatch ((has-all has-all) value)
  (loop for matcher in (matchers has-all)
        unless (matchesp matcher value)
          return (describe-mismatch matcher value)
        finally
        (return "no mismatch")))

(defmethod matchesp ((has-any has-any) value)
  (loop for matcher in (matchers has-any)
        if (matchesp matcher value)
          return t))

(defun check-matcher-list (matchers)
  (dolist (x matchers)
    (check-type x matcher)))

(defun has-all (&rest matchers)
  (check-matcher-list matchers)
  (make-instance 'has-all :matchers matchers))

(defun has-any (&rest matchers)
  (check-matcher-list matchers)
  (make-instance 'has-any :matchers matchers))

(defclass has-typep (single-value-matcher)
  ())

(defun has-typep (type)
  (make-instance 'has-typep :value type))

(defmethod matchesp ((matcher has-typep) value)
  (typep value (value matcher)))

(defmethod describe-self ((matcher has-typep))
  `("has type " ,(value matcher)))

(Defmethod describe-mismatch ((matcher has-typep) value)
  `("has type " ,(type-of value)))

(defmethod describe-mismatch-to-string (matcher value)
  (format
   nil
   "Expected: ~a
     but: ~a"
   (with-output-to-string (s)
     (format-description (describe-self matcher) s))
   (with-output-to-string (s)
     (format-description (describe-mismatch matcher value) s))))

(defun call-assert-that (value matcher expression match-expression)
  (declare (optimize (debug 3) (speed 0))) ;; Keep everything in the stacktrace!
  (cond
    ((matchesp matcher value)
     (fiveam::add-result 'fiveam::test-passed
                         :test-expr `(assert-that
                                      ,expression
                                      ,match-expression)))
    (t
     (fiveam::process-failure
      `(assert-that
        ,expression
        ,match-expression)
      "~a"
      (describe-mismatch-to-string
       matcher value)))))

(defmethod describe-self-to-string ((self matcher))
  (with-output-to-string (s)
    (format-description (describe-self self) s)))

(defmacro assert-that (value &rest matchers)
  (alexandria:with-gensyms (value-sym)
   `(let ((,value-sym ,value))
      ,@(loop for matcher in matchers collect
            `(call-assert-that
              ,value-sym
              ,matcher
              ',value
              ',matcher)))))

(defclass error-with-string-matching (matcher)
  ((delegate :initarg :delegate
             :reader delegate)))

(defmethod matchesp ((matcher error-with-string-matching) value)
  (and
   (typep value 'error)
   (matchesp
    (delegate matcher)
    (princ-to-string value))))

(defmethod describe-mismatch-to-string ((matcher error-with-string-matching) value)
  (cond
    ((not (typep value 'error))
     (format nil "Wasn't provided an error type"))
    (t
     (format nil "Expected error with message to match: ~a"
             (describe-mismatch-to-string (delegate matcher) (princ-to-string value))))))

(defmethod describe-self ((matcher error-with-string-matching))
  (format nil "An error with message: ~a"
          (describe-self (delegate matcher))))

(defmethod error-with-string-matching (matcher)
  (make-instance 'error-with-string-matching :delegate matcher))

(defmethod error-with-string-matching ((matcher string))
  (error-with-string-matching (equal-to matcher)))

(defmacro signals-error-matching ((&optional (error-class 'simple-error)) expr &rest matchers)
  "Checks if a the expr signaled an error whose string representation
matches the given matchers. Useful for asserting SIMPLE-ERRORs."
  `(progn
     (let ((no-error nil))
      (handler-case
          (progn
            ,expr
            (setf no-error t))
        (,error-class (e)
          (assert-that
           e
           ,@matchers)))
       (if no-error
           (fiveam:fail "Expected to see an exception when running ~a, but didn't see any" ',expr)))))
