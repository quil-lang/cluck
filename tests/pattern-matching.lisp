(in-package :cluck/tests)

;;;;;;;;;;;;;;;;;;;;
;; NAIVE PATTERNS ;;
;;;;;;;;;;;;;;;;;;;;

(defun naive-match (eg pat vars)
  (e-graph-match eg (make-naive-pattern pat vars)))

(deftest basic-patterns ()
  (with-e-graph eg
    (let* ((two (e-add '(2)))
           (one (e-add '(1)))
           (plus-two (e-add `(+ ,two))))
      (is (equal `((,two)) (naive-match eg 2 '())))
      (is (equal `((,one)) (naive-match eg 1 '())))
      (is (equal `((,plus-two ,two)) (naive-match eg '(+ x) '(x))))
      (is (equal `((,plus-two)) (naive-match eg '(+ _) '()))))))

(deftest arity-patterns ()
  "Pattern shouldn't match application with different arity"
  (with-e-graph eg
    (let* ((one (e-add '(1)))
           (two (e-add '(2)))
           (three (e-add '(3)))
           (plus-one (e-add `(+ ,one)))
           (plus-one-two (e-add `(+ ,one ,two)))
           (plus-one-two-three (e-add `(+ ,one ,two ,three))))
      (is (equal `((,plus-one ,one)) (naive-match eg `(+ x) '(x))))
      (is (equal `((,plus-one-two ,one ,two)) (naive-match eg `(+ x y) '(x y))))
      (is (equal `((,plus-one-two-three ,one ,two ,three)) (naive-match eg `(+ x y z) '(x y z))))

      (is (equal `((,plus-one-two ,two)) (naive-match eg `(+ _ y) '(y))))
      (is (equal `((,plus-one-two)) (naive-match eg `(+ _ _) '())))
      (is (equal `((,plus-one)) (naive-match eg `(+ _) '()))))))

(deftest matching-symbols ()
  "Match a symbol literally when it's not in the return list."
  (with-e-graph eg
    (e-add '(x))
    (e-add '(5))
    (is (= 2 (length (naive-match eg 'x '(x)))))
    (is (= 1 (length (naive-match eg 'x '()))))
    (is (= 0 (length (naive-match eg 'y '()))))))

(deftest matching-multiple ()
  "Match the same variable multiple times, ensure that it's forced to stay the same."
  (with-e-graph eg
    (e-add '(+ (5) (* (3) (9))))
    (let ((minus-node (e-add '(- (5) (/ (5) (9)))))
          (five-node (e-add '(5))))
      (is (null (e-graph-match eg (make-naive-pattern '(+ x (* x _)) '(x)))))
      (is (equal `((,minus-node ,five-node))
                 (e-graph-match eg (make-naive-pattern '(- x (/ x _)) '(x))))))))

(deftest identical-matches ()
  (with-e-graph eg
    (let* ((node-1 (e-add '(* (5) (0))))
           (node-2 (e-add '(* (5) (3))))
           (_ (e-merge node-1 node-2))
           (merged-ecid (e-graph-canonical-e-class-id eg node-1))
           (five-ecid (e-add '(5))))
      (declare (ignore _))
      (is (equal `((,merged-ecid ,five-ecid))
                 (e-graph-match eg (make-naive-pattern '(* x _) '(x))))))))

(deftest hang-regression ()
  (with-e-graph eg
    (let ((zero-pat (make-naive-pattern '(* _ 0) '()))
          (zero-ecid (e-add '(0)))
          (zero-ecid-1 (e-add '(* (5) (0))))
          (zero-ecid-2 (e-add '(* (0) (5))))
          (zero-ecid-3 (e-add '(* (0) (* (5) (5)))))
          (zero-ecid-4 (e-add '(* (* (5) (5)) (0)))))
      (e-merge zero-ecid zero-ecid-1)
      (e-merge zero-ecid zero-ecid-2)
      (e-merge zero-ecid zero-ecid-3)
      (e-merge zero-ecid zero-ecid-4)
      (e-graph-match eg zero-pat))))

;;;;;;;;;;;;;;;;;;;;
;; GRAPH PATTERNS ;;
;;;;;;;;;;;;;;;;;;;;

(defun binding= (b1 b2)
  "Check whether two binding alists are equal."
  (let ((keys1 (remove-duplicates (mapcar #'car b1)))
        (keys2 (remove-duplicates (mapcar #'car b2))))
    (and (set-equal keys1 keys2)
         (loop :for key1 :in keys1
               ;; binding should be e-nodes or e-class IDs, both of which can be EQUAL'd
               ;; in fact, we could make things work with EQL, but EQUAL makes it so we don't have to fetch the exact e-node in our "expected".
               :always (equal (cdr (assoc key1 b1)) (cdr (assoc key1 b2)))))))

(defun bindings= (b1 b2)
  (set-equal b1 b2 :test 'binding=))

(deftest linear-graph-pattern ()
  (with-e-graph eg
    (let* ((two (e-add '(2)))
           (one (e-add '(1)))
           (plus-two (e-add '(+ (2))))

           (two-pat (make-instance 'graph-pattern
                                   :binding-restrictions (plist-hash-table '(a (2)))))
           (plus-two-pat (make-instance 'graph-pattern
                                        :binding-restrictions (plist-hash-table '(a (+ b)))))
           ;; just to test multiple restrictions
           (other-plus-two-pat (make-instance 'graph-pattern
                                              :binding-restrictions
                                              (plist-hash-table '(a (+ b) b (2)))))
           (two-pat-bindings (e-graph-match eg two-pat))
           (expected-two-pat-bindings '(((a . (2)))))
           (plus-two-pat-bindings (e-graph-match eg plus-two-pat))
           (expected-plus-two-pat-bindings `(((a . (+ ,two)) (b . ,two))))
           (other-plus-two-pat-bindings (e-graph-match eg other-plus-two-pat))
           ;; since b is now restricted, it will be bound to the e-node, not ecid
           (expected-other-plus-two-pat-bindings `(((a . (+ ,two)) (b  . (2))))))
      (declare (ignore one plus-two))
      (is (bindings= expected-two-pat-bindings two-pat-bindings))
      (is (bindings= expected-plus-two-pat-bindings plus-two-pat-bindings))
      (is (bindings= expected-other-plus-two-pat-bindings other-plus-two-pat-bindings)))))

(deftest multiple-roots-graph-pattern ()
  (with-e-graph eg
    (let* ((two (e-add '(2)))
           (one (e-add '(1)))
           (plus-two (e-add '(+ (2))))
           (times-two (e-add '(* (2))))
           (plus-one (e-add '(+ (1))))
           (times-one (e-add '(* (1))))

           (plus-times-pat
             (make-instance 'graph-pattern
                            :binding-restrictions (plist-hash-table '(a (2)
                                                                      b (+ a)
                                                                      c (* a)))))
           (plus-times-pat-bindings (e-graph-match eg plus-times-pat))
           (expected-plus-times-pat-bindings `(((a . (2))
                                                (b . (+ ,two))
                                                (c . (* ,two))))))

      (declare (ignore one plus-one times-one ; just in there to make sure we don't over-match
                       plus-two times-two))
      (is (bindings= expected-plus-times-pat-bindings plus-times-pat-bindings)))))

(deftest duplicate-children-pattern ()
  (with-e-graph eg
    (let* ((two (e-add '(2)))
           (plus-two-two (e-add '(+ (2) (2))))
           (plus-one-two (e-add '(+ (1) (2))))
           (plus-two-one (e-add '(+ (2) (1))))

           (pat
             (make-instance 'graph-pattern
                            :binding-restrictions (plist-hash-table '(p (+ a a)))))
           (pat-bindings (e-graph-match eg pat))
           (expected-bindings `(((a . ,two)
                                 (p . (+ ,two ,two))))))
      (declare (ignore plus-one-two plus-two-one plus-two-two))
      (is (bindings= expected-bindings pat-bindings)))))

(deftest graph-pattern-vs-loop ()
  (with-e-graph eg
    (let* ((five (e-add '(5)))
           (divide-5 (e-add '(/ (5))))
           (plus (e-add `(+ ,divide-5)))
           (times (e-add `(* ,plus)))
           (_ (e-merge divide-5 times))
           (__ (e-graph-rebuild eg))
           (divide-5 (e-graph-canonical-e-class-id eg divide-5))
           (pat-1
             (make-instance 'graph-pattern
                            :binding-restrictions (plist-hash-table '(x (/ j)
                                                                      a (+ x)
                                                                      b (* a)
                                                                      c (+ b)
                                                                      d (* c)
                                                                      e (+ d)))))
           (pat-1-bindings (e-graph-match eg pat-1))
           (pat-1-expected-bindings `(((j . ,five)
                                       (x . (/ ,five))
                                       (a . #1=(+ ,divide-5))
                                       (b . #2=(* ,plus))
                                       (c . #1#)
                                       (d . #2#)
                                       (e . #1#)))))
      (declare (ignore _ __))
      (is (bindings= pat-1-expected-bindings pat-1-bindings)))))

(deftest nonlinear-graph-pattern ())

(deftest custom-car-matcher ()
  "Test that custom car matchers are enforced, and also that the match is aborted if two car matchers produce conflicting bindings"
  (flet ((car-match-allow-bind (binding-car actual-car)
           "Allows bindings of the form (:exact sym) or (:bind sym). The former requires EQL between sym and the actual car and returns no bindings. The latter binds the car to the given symbol and never fails to match."
           (ecase (car binding-car)
             (:exact
              (if (eql (cadr binding-car) actual-car)
                  nil
                  'no-match))
             (:bind
                 ;; emacs is being weird about the indentation here, something specific about :bind
                 `((,(cadr binding-car) . ,actual-car)))))
         (car-match-multiple (binding-car actual-car)
           (if (integerp (/ actual-car binding-car))
               nil
               'no-match)))
    (with-e-graph eg
      (let* ((two (e-add '(2)))
             (one (e-add '(1)))
             (one-plus-two (e-add '(+ (1) (2))))
             (one-plus-one (e-add '(+ (1) (1))))
             ;; Basic test that custom car binders work at all
             (bind-to-num-pat (make-instance 'graph-pattern
                                             :binding-restrictions (plist-hash-table '(a ((:bind num))))
                                             :car-match-fn #'car-match-allow-bind))
             (bind-to-num-expected '(((a . (1)) (num . 1)) ((a . (2)) (num . 2))))
             ;; Test that all car binders are forced to take on the same value.
             (plus-a-a-pat (make-instance 'graph-pattern
                                          :binding-restrictions
                                          (plist-hash-table '(a ((:exact +) b c)
                                                              b ((:bind num))
                                                              c ((:bind num))))
                                          :car-match-fn #'car-match-allow-bind))
             (plus-a-a-expected `(((a . (+ ,one ,one)) (num . 1) (b . (1)) (c . (1))))))

        (declare (ignorable one two one-plus-one one-plus-two))
        (is (bindings= bind-to-num-expected (e-graph-match eg bind-to-num-pat)))
        (is (bindings= plus-a-a-expected (e-graph-match eg plus-a-a-pat)))))))

(deftest multiple-matches ())
