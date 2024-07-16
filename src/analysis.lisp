;;;; Author: Mark Polyakov, released under MIT License

(in-package #:cluck)

(defclass e-analyzer ()
  ()
  (:documentation "Simple e-class analyses may be created by creating a subclass of E-CLASS-ANALYZER which implements E-CLASS-ANALYZER-ATOM and E-CLASS-ANALYZER-MERGE. More complex analyses may be implemented by subclassing E-CLASS directly."))

(defclass e-analyzer-cost (e-analyzer)
  ()
  (:documentation "An e-class analyzer which can be used as the cost function when determining the best concrete representation of an e-class. Should implement E-CLASS-ANALYZER-COMPARE. A canned implementation of E-ANALYZER-MERGE exists for instances, which simply finds the cheapest e-node according to the analysis value; this implementation must not be overridden."))

(defgeneric e-analyzer-e-node-value (analyzer e-node-car children)
  (:documentation "Return the analysis value for an e-node. CHILDREN is a list of analysis values of the children. Some elements of CHILDREN may have circular references back to the e-class containing this e-node. In that case, E-ANALYZER-PARENT-VALUE is called to determine a reasonable default value. Note that in some situations, a circular reference will not use the parent value, depending on order of traversal, and instead will just be the value not including this new node."))
(defgeneric e-analyzer-compare (analyzer v1 v2)
  (:documentation "Compare two values. Return non-nil iff v1 is strictly less than v2."))

(defgeneric e-analyzer-merge (analyzer v1 v2)
  (:documentation "Merge the analysis values v1 and v2 into a single value. Ie, if v1 and v2 are values for e-nodes in the same e-class, this method should return the value of the e-class. If v1 and v2 represent costs, that would be the minimum.")
  (:method ((a e-analyzer-cost) v1 v2)
    (if (e-analyzer-compare a v1 v2) v1 v2)))

(defgeneric e-analyzer-parent-value (analyzer)
  (:documentation "See E-ANALYZER-E-NODE-VALUE. Generally speaking, should return some logically maximum/high-cost value for cost analyzers, since a circular reference has infinite size.")
  (:method (a)
    'parent))

(defclass tree-size-analyzer (e-analyzer-cost)
  ()
  (:documentation "An e-class analyzer that simply counts the smallest representation of each e-class."))
(defvar +tree-size-analyzer+ (make-instance 'tree-size-analyzer))
(defmethod e-analyzer-e-node-value ((a tree-size-analyzer) fn child-costs)
  (1+ (apply #'+ child-costs)))
(defmethod e-analyzer-compare ((a tree-size-analyzer) v1 v2)
  (< v1 v2))
(defmethod e-analyzer-parent-value ((a tree-size-analyzer))
  most-positive-fixnum)

(defclass tree-depth-analyzer (e-analyzer-cost)
  ()
  (:documentation "An e-class analyzer that counts the smallest depth of any representation of each e-class."))
(defvar +tree-depth-analyzer+ (make-instance 'tree-depth-analyzer))
(defmethod e-analyzer-e-node-value ((a tree-depth-analyzer) fn child-costs)
  (1+ (apply #'min child-costs)))
(defmethod e-analyzer-compare ((a tree-depth-analyzer) v1 v2)
  (< v1 v2))
(defmethod e-analyzer-parent-value ((a tree-depth-analyzer))
  most-positive-fixnum) ; won't cause overflow because we only ever decrease it (not that perf
                                        ; matters a whole ton right here).

(declftype (e-graph e-class-id e-analyzer-cost) generalized-e-node e-graph-e-class-cheapest-e-node)
(defun e-graph-cheapest-representation-by-analyzer (eg ecid anal)
  "Find the cheapest representation of the given e-class in the e-graph. As a second value, returns that cost.

The cost is determined as the lowest according to ANALYZER, which must be a subclass of E-CLASS-ANALYZER-COMPARE. This will cause the value to be calculated for every node in the e-graph, which may be slow for a large graph. If you wish to incrementally calculate the best representative node in each e-class, implement a proper incremental e-analysis."
  (let ((class-costs (make-hash-table)) ; actually, hash values are (cons node cost)
        (class-expansions (make-hash-table)))
    (labels ((cheapest-representation (ecid)
               "Fill out the CLASS-COSTS hash table for ecid (and any nodes reachable from it)"
               (let ((ec (e-graph-e-class-id->e-class eg ecid)))
                 (multiple-value-bind (val exists-p) (gethash ec class-costs)
                   (cond
                     (exists-p val)
                     (t
                      ;; temp value for cycle detection
                      (setf (gethash ec class-costs) (cons nil (e-analyzer-parent-value anal)))
                      (setf (gethash ec class-costs)
                            ;; Possible optimization, just minimize during the loop, maybe using ITERATE?
                            (extremum
                             (loop :for node :in (e-class-e-nodes ec)
                                   :for child-costs := (loop :for child-ecid :in (cdr node)
                                                             :collect (cdr (cheapest-representation child-ecid)))
                                   :collect (cons node (e-analyzer-e-node-value anal (car node) child-costs)))
                             (curry #'e-analyzer-compare anal)
                             :key #'cdr)))))))

             (expand-representation (ecid)
               "Get the fully expanded cheapest representation of ecid (as a generalized e-node with no e-class IDs)"
               (let ((en (car (gethash (e-graph-e-class-id->e-class eg ecid) class-costs))))
                 (etypecase en
                   (e-node-atom
                    en)
                   (e-node
                    ;; using a hash table enables memory sharing in expanded output
                    (or (gethash ecid class-expansions)
                        (setf (gethash ecid class-expansions)
                              (cons (car en) (mapcar #'expand-representation (cdr en))))))))))

      (cheapest-representation ecid)
      (values (expand-representation ecid) (cdr (gethash (e-graph-e-class-id->e-class eg ecid) class-costs))))))
