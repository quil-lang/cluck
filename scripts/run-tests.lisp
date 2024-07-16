
(defpackage #:cluck.run-tests
  (:documentation "Helper to run the tests from the command line.")
  (:use #:cl))

(in-package #:cluck.run-tests)

(asdf:initialize-source-registry
 `(:source-registry
   (:directory ,(uiop:pathname-parent-directory-pathname *load-pathname*))
   :inherit-configuration))

;; for debugging
;; (print (asdf:locate-system "cluck"))

(asdf:test-system "cluck")
