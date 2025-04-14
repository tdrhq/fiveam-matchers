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
               (:file "numbers")
               (:file "misc")
               (:file "strings")
               (:file "satisfying")
               (:file "errors")
               (:file "all")))

(defsystem :fiveam-matchers/markup
  :serial t
  :depends-on (:fiveam-matchers
               :markup)
  :components ((:module "markup"
                :components ((:file "package")
                             (:file "tags")))))

(defsystem :fiveam-matchers/tests
  :serial t
  :depends-on (:fiveam-matchers
               :fiveam-matchers/markup)
  :components ((:file "test-core")
               (:file "test-lists")
               (:file "test-strings")
               (:file "test-errors")
               (:file "test-satisfying")
               (:module "markup"
                :components ((:file "test-tags")))))
