(defsystem :fiveam-matchers
  :serial t
  :depends-on (:fiveam
               :cl-ppcre
               :str)
  :description "An extensible matchers library for FiveAM"
  :author "Arnold Noronha <arnold@tdrhq.com>"
  :license  "Apache License, Version 2.0"
  :version "0.0.1"

  :components ((:file "core")
               (:file "delegating")
               (:file "described-as")
               (:file "every-item")
               (:file "lists")
               (:file "has-length")
               (:file "misc")
               (:file "strings")
               (:file "satisfying")
               (:file "all")))

(defsystem :fiveam-matchers/tests
  :serial t
  :depends-on (:fiveam-matchers)
  :components ((:file "test-core")
               (:file "test-lists")
               (:file "test-strings")
               (:file "test-satisfying")))
