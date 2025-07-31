(defpackage #:cluck/examples/cas
  (:documentation "Computer Algebra System using the CLuck e-graph library")
  (:use #:cl #:cluck)
  (:export #:simplify
           ;; Simplification strategies:
           #:simplify-shortest
           #:simplify-shallowest
           #:simplify-factor))
