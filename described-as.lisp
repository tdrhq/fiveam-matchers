(defpackage :fiveam-matchers/described-as
  (:use #:cl)
  (:import-from #:fiveam-matchers/delegating
                #:delegating-matcher)
  (:import-from #:fiveam-matchers/core
                #:describe-self)
  (:local-nicknames (#:a #:alexandria))
  (:export
   #:described-as))
(in-package :fiveam-matchers/described-as)

;; The reason this is a macro and not a function is just because of
;; the indentation. Emacs will indent this in a better way than if
;; these were arguments.
(defmacro described-as (message &body body)
  "Override the description for a given matcher"
  `(call-described-as ,message ,@body))

(defun call-described-as (message matcher)
  (assert matcher)
  (make-instance
   'delegating-matcher
   :matcher matcher
   :describe-self (lambda ()
                    `(,message ": " ,(describe-self matcher)))))
