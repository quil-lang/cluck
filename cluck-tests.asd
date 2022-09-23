(asdf:defsystem :cluck-tests
  :description "Tests for CLuck"
  :author "Mark Polyakov <mapolyakov@hrl.com>"
  :license "MIT"
  :depends-on (:cluck
               :alexandria
               :fiasco
               :trivial-garbage)
  :pathname "tests/"
  :serial t
  :components ((:file "package")
               (:file "utils")
               (:file "essential-operations")
               (:file "pattern-matching")
               (:file "minutiae")))
