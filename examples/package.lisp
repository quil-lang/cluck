(defpackage #:cluck/examples/cas
  (:use #:cl #:cluck)
  (:export #:simplify
           ;; Simplification strategies:
           #:simplify-shortest
           #:simplify-shallowest
           #:simplify-factor))
