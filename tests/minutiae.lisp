;; Tests for optional features, optimizations, etc

(in-package :cluck/tests)

(deftest extraction-memory-sharing ()
  "When multiple e-nodes point to the same e-class, the representation of that e-class shall be shared in the extracted representation."
  (with-e-graph eg
    (let* ((one (e-add '(1)))
           (plus-one (e-add `(+ ,one)))
           (times-one (e-add `(* ,one)))
           (root (e-add `(+ ,plus-one ,times-one)))
           (extracted (cluck:e-graph-cheapest-representation-by-analyzer eg root +tree-size-analyzer+)))
      (is (equal '(1) (cadadr extracted))) ; sanity check
      (is (equal '(1) (cadadr (cdr extracted))))
      (is (eq (cadadr extracted) (cadadr (cdr extracted)))))))

#+(or allegro ccl cmu lispworks sbcl)
(progn
  (defclass custom-car-e-graph (e-graph)
    ()
    (:default-initargs
     :e-node-car=-fn #'string=
     :e-node-car-hash-fn (compose #'sxhash #'string)))

  (deftest custom-car-test ()
    ;; TODO: this test needs to be improved because we presently have bugs related to custom car but this test ain't catching them
    (let* ((normal-eg (make-instance 'e-graph)) ; just to double check that we're actually seeing different behavior
           (custom-eg (make-instance 'custom-car-e-graph))
           (ecid-1 (e-graph-add normal-eg '("HELLO")))
           (ecid-2 (e-graph-add normal-eg '(hello)))
           (ecid-3 (e-graph-add custom-eg '("HELLO")))
           (ecid-4 (e-graph-add custom-eg '(hello)))
           (one (e-graph-add custom-eg '(one)))
           (two (e-graph-add custom-eg '(two)))
           (hello-one (e-graph-add custom-eg `("HELLO" ,one)))
           (hello-one-prime (e-graph-add custom-eg `(hello ,two))))
      (is (/= ecid-1 ecid-2))
      (is (= ecid-3 ecid-4)) ; really should be equal, not just e-class-id=, because no merging involved
      (is (/= hello-one hello-one-prime))
      (e-graph-merge custom-eg one two)
      (e-graph-rebuild custom-eg)
      (is (e-graph-e-class-id= custom-eg hello-one hello-one-prime)))))
