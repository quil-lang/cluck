(asdf:defsystem :cluck-tests
  :description "Tests for CLuck"
  :author "Mark Polyakov <mapolyakov@hrl.com>"
  :license "MIT"
  :depends-on (:cluck
               :alexandria
               :fiasco
               :trivial-garbage)
  :perform (test-op (op system)
                    (symbol-call :fiasco '#:run-package-tests
                                 :package '#:cluck/tests))
  :pathname "tests/"
  :serial t
  :components ((:file "package")
               (:file "utils")
               (:file "essential-operations")
               (:file "pattern-matching")
               (:file "minutiae")))
