(in-package #:cluck/tests)

(deftest basic-adding-only ()
  (with-e-graph eg
    (let* ((plus-class (e-add '(+)))
           (plus-class-2 (e-add '(+)))
           (times-class (e-add '(*)))
           (minus-class (e-add `(- ,plus-class)))
           (minus-class-2 (e-add `(- ,plus-class))))
      (is (= plus-class plus-class-2))
      (is (/= plus-class times-class))
      (is (= minus-class minus-class-2))
      (is (/= plus-class times-class minus-class))
      (check-e-graph eg))))

(deftest basic-merging ()
  (with-e-graph eg
    (let* ((plus-class (e-add '(+)))
           (times-class (e-add '(*)))
           (_ (e-merge plus-class times-class))
           (new-plus-class (e-graph-canonical-e-class-id eg plus-class))
           (new-plus-class-2 (e-add '(+)))
           (new-times-class (e-graph-canonical-e-class-id eg times-class))
           (new-times-class-2 (e-add '(*))))
      (declare (ignore _))

      (e-graph-rebuild eg)
      (is (e-graph-e-class-id= eg
                               new-plus-class new-plus-class-2
                               new-times-class new-times-class-2))
      (check-e-graph eg))))

(deftest nontrivial-merging ()
  (with-e-graph eg
    (let* ((three-class (e-add '(3)))
           (four-class (e-add '(4)))
           (plus-3-class (e-add `(+ ,three-class)))
           (plus-4-class (e-add `(+ ,four-class)))
           (_ (e-merge three-class four-class))
           (new-plus-3-class (e-add `(+ ,three-class)))
           (new-plus-3-class-2 (e-graph-canonical-e-class-id eg plus-3-class))
           (new-plus-4-class (e-add `(+ ,four-class)))
           (new-plus-4-class-2 (e-graph-canonical-e-class-id eg plus-4-class)))

      (declare (ignore _))
      (e-graph-rebuild eg)
      (is (e-graph-e-class-id= eg
                               new-plus-3-class new-plus-3-class-2
                               new-plus-4-class new-plus-4-class-2))
      (check-e-graph eg))))

(deftest e-class-e-nodes-regression ()
  "Regression test to ensure that e-class-e-nodes is kept canonical through rebuilding."
  (with-e-graph eg
    (let* ((one-ecid (e-add '(1)))
           (two-ecid (e-add '(2)))
           ;; two different parents, to ensure that at least one of them would not be canonical if
           ;; no special care were taken.
           (plus-one-ecid (e-add '(+ (1))))
           (times-two-ecid (e-add '(* (2))))
           (_ (e-merge one-ecid two-ecid))
           (__ (e-graph-rebuild eg))
           (canonical-num-ecid (e-graph-canonical-e-class-id eg one-ecid)))
      (declare (ignore _ __))
      (is (equal `((+ ,canonical-num-ecid)) (e-class-e-nodes (e-graph-e-class-id->e-class eg plus-one-ecid))))
      (is (equal `((* ,canonical-num-ecid)) (e-class-e-nodes (e-graph-e-class-id->e-class eg times-two-ecid)))))))

(deftest grandparent-merge ()
  (with-e-graph eg
    (let* ((1-class (e-add '(1)))
           (2-class (e-add '(2)))
           (plus-class (e-add `(+ ,1-class)))
           (plus-class-2 (e-add `(+ ,2-class)))
           (times-class (e-add `(* ,plus-class)))
           (times-class-2 (e-add `(* ,plus-class-2))))

      (check-e-graph eg)
      ;; Could/should be improved to use e-graph-e-class-id
      (is (/= 1-class 2-class plus-class plus-class-2 times-class times-class-2))
      (e-merge 1-class 2-class)
      (e-graph-rebuild eg)
      (is (e-graph-e-class-id= eg 1-class 2-class))
      (is (e-graph-e-class-id= eg plus-class plus-class-2))
      (is (e-graph-e-class-id= eg times-class times-class-2))
      (check-e-graph eg))))

(deftest random-adds-merges (&key noise (num-repetitions 100) (num-ops 100))
  (let ((merge-probability 1/10)
        (max-num-args 5)
        (fn-symbols '(+ * /)))
    (dotimes (i num-repetitions)
      (format noise "~%Repetition ~a:~%" i)
      (with-e-graph eg
        (let (e-class-ids)
          (dotimes (j num-ops)
            (cond
              ((and (>= (length e-class-ids) 2)
                    (biased-coin-flip merge-probability))
               (let ((ecid1 (random-elt e-class-ids))
                     (ecid2 (random-elt e-class-ids)))
                 (format noise "Merging ~a and ~a.~%" ecid1 ecid2)
                 (e-merge ecid1 ecid2)))

              (t ; add node
               (let* ((num-args (random (1+ max-num-args)))
                      (args (when e-class-ids
                              (loop :repeat num-args
                                    :collect (random-elt e-class-ids))))
                      (fn-symbol (random-elt fn-symbols))
                      (new-ecid (e-add (cons fn-symbol args))))
                 (format noise "Created ~a into e-class ~a.~%" (cons fn-symbol args) new-ecid)
                 ;; room for improvement: As e-classes get merged together, they'll be duplicated in
                 ;; the list and therefore more likely to be chosen again. Big e-classes get bigger.
                 (push new-ecid e-class-ids)))))
            ;; Could move this out of loop if slow
            (e-graph-rebuild eg)
            (check-e-graph eg))))))

;; TODO circular?
