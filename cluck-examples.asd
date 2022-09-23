(asdf:defsystem :cluck-examples
  :description "Examples for CLuck"
  :author "Mark Polyakov <mapolyakov@hrl.com>"
  :license "MIT"
  :depends-on (:alexandria
               :cluck)
  :pathname "examples/"
  :serial t
  :components ((:file "package")
               (:file "cas")))
