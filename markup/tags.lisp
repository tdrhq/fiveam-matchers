;;;; Copyright 2018-Present Modern Interpreters Inc.
;;;;
;;;; This Source Code Form is subject to the terms of the Mozilla Public
;;;; License, v. 2.0. If a copy of the MPL was not distributed with this
;;;; file, You can obtain one at https://mozilla.org/MPL/2.0/.

(defpackage :fiveam-matchers/markup/tags
  (:use #:cl)
  (:import-from #:fiveam-matchers/markup
                #:has-matching-descendent)
  (:import-from #:fiveam-matchers/core
                #:matcher
                #:describe-self
                #:matchesp
                #:ensure-matcher)
  (:import-from #:markup
                #:xml-merge-tag-children
                #:xml-merge-tag
                #:merge-tag
                #:xml-tag-children
                #:abstract-xml-tag))
(in-package :fiveam-matchers/markup/tags)

(defclass has-matching-descendent (matcher)
  ((matcher :initarg :matcher
            :reader delegate)))

(defun has-matching-descendent (val)
  (make-instance 'has-matching-descendent :matcher (ensure-matcher val)))

(defun have-matching-descendent (val)
  (has-matching-descendent val))

(defmethod matchesp ((self has-matching-descendent) (x abstract-xml-tag))
  (or
   (matchesp (delegate self) x)
   (loop for child in (xml-tag-children x)
         if (matchesp self child)
           return t)))

(defmethod matchesp ((self has-matching-descendent) (x markup/markup::unescaped-string))
  (matchesp (delegate self) (markup/markup::unescaped-string-content x)))

(defmethod matchesp ((self has-matching-descendent) (x xml-merge-tag))
  (loop for child in (xml-merge-tag-children x)
        if (matchesp self child)
           return t))

(defmethod matchesp ((self has-matching-descendent) (x string))
  (matchesp (delegate self) x))

(defmethod describe-self ((self has-matching-descendent))
  `("Has a descendent that matches " ,(describe-self (delegate self))))
