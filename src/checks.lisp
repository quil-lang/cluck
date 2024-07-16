;;;; Author: Mark Polyakov, released under MIT License

(in-package #:cluck)

(declftype (e-graph) t check-e-graph-hashcons-invariant)
(defun check-e-graph-hashcons-invariant (eg)
  "Throw an error if the hashcons invariant does not hold"
  ;; For each e-node, make sure its canonical version is in the
  ;; hashcons and points to the right place.  In the end, check that
  ;; there are no extraneous entries in the hashcons.
  (let (expected-hashcons-keys
        (hashcons (e-graph-e-node-e-classes eg)))
    (loop :for ecid :in (e-graph-e-class-id-list eg)
          ;; will error here if ecid is not an integer (eg, if nil)
          :for ec := (e-graph-e-class-id->e-class eg ecid)
          :do (loop :for en :in (e-class-e-nodes ec)
                    :for canonical-e-node := (e-graph-canonical-e-node eg en)
                    :do (push canonical-e-node expected-hashcons-keys)
                    :when (not ec)
                      :do (error "Hashcons invariant violated: e-node missing from hashcons.")
                    :when (not (eq ec (e-graph-e-class-id->e-class
                                       eg
                                       ;; avoid re-canonicalizing e-node
                                       (gethash canonical-e-node hashcons))))
                      :do (error "Hashcons invariant violated: e-node pointed to wrong e-class.")))
    (when (not (set-equal expected-hashcons-keys (hash-table-keys (e-graph-e-node-e-classes eg))
                          :test (e-graph-e-node-car=-fn eg)))
      (error "Hashcons invariant violated: Unexpected keys in hashcons."))
    t))

(declftype (e-graph) t check-e-graph-congruence-invariant)
(defun check-e-graph-congruence-invariant (eg)
  "Throw an error if the congruence variant does not hold; that is, if two congruent e-nodes are stored separately in the e-graph. This check is only sound if the hashcons invariant holds."
  (loop :with seen-canonical-e-nodes := (funcall (e-graph-make-e-node-hash-table-fn eg))
        :for ecid :in (e-graph-e-class-id-list eg)
        :for ec := (e-graph-e-class-id->e-class eg ecid)
        :do (loop :for en :in (e-class-e-nodes ec)
                  :for canonical-en := (e-graph-canonical-e-node eg en)
                  :when (gethash canonical-en seen-canonical-e-nodes)
                    :do (error "Congruence invariant violated: Multiple e-nodes congruent to ~a." canonical-en))))

(declftype (e-graph) boolean check-e-graph)
(defun check-e-graph (eg)
  "Throw a error if the given e-graph violates its key invariants."
  (check-e-graph-hashcons-invariant eg)
  (check-e-graph-congruence-invariant eg)
  t)

;; TODO: add a function to print all representations of an
;; e-class. Will be a bit annoying to deal with circular graphs. See
;; https://github.com/quil-lang/cluck/issues/5
