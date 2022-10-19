;; Copyright (c) Mark Polyakov, released under MIT License

(in-package :cluck)

(defvar *rewrite-rules* (make-hash-table))

;; An enode is represented as a list whose CAR is a symbol denoting a function and whose CDR is a list of e-class IDs

(deftype e-class-id ()
  "A (non-unique) NUMBER which identifies an e-class."
  'union-find-id)

(declftype (t) boolean e-class-id-p)
(defun e-class-id-p (ecid)
  (typep ecid 'e-class-id))

(declftype (t) boolean e-node-p)
(defun e-node-p (en)
  (and (consp en)
       (atom (car en))
       (every #'e-class-id-p (cdr en))))

(deftype e-node ()
  '(and cons (satisfies e-node-p)))

(declftype (t) boolean generalized-e-node-p)
(defun generalized-e-node-p (en)
  (or (e-class-id-p en)
      (and (consp en)
           (atom (car en))
           (loop :for c :in (cdr en)
                 :always (or (e-class-id-p c) (generalized-e-node-p c))))))

(deftype generalized-e-node ()
  "A generalized e-node is either an e-node, an e-class ID, or an e-node-like form that may contain other generalized e-nodes as children."
  '(or e-class-id (satisfies generalized-e-node-p)))

(declftype (e-node) boolean e-node-atom-p)
(defun e-node-atom-p (en)
  (and (e-node-p en) (not (cdr en))))

(deftype e-node-atom ()
  '(and e-node (satisfies e-node-atom-p)))

(declftype (e-node) atom e-node-fn)
(defun e-node-fn (en)
  (car en))

(defclass e-class ()
  ((e-nodes :accessor e-class-e-nodes :initform nil :initarg :e-nodes :type list)
   (parents :accessor e-class-parents :initform (error "e-class-parents req'd") :initarg :parents :type hash-table
            :documentation "Hash table whose keys are our parent e-nodes.")))

(declftype (hash-table hash-table &optional hash-table) hash-table hash-table-union)
(defun hash-table-union (ht1 ht2
                         &optional (ht-result (make-hash-table :test (hash-table-test ht1))))
  "Return a new hash table having all the keys of both HT1 and HT2. Values are undefined. Does not mutate its parameters. HT-RESULT should be an empty hash table that the intersection will be stored into. (This allows full customization of the hash table options)."
  ;; this assertion isn't strictly required but tends to be something we want
  (assert (eql (hash-table-test ht1) (hash-table-test ht2)) () "Arguments to HASH-TABLE-UNION must use the same test.")
  (loop :for k :being :each :hash-key :of ht1
        :do (setf (gethash k ht-result) t))
  (loop :for k :being :each :hash-key :of ht2
        :do (setf (gethash k ht-result) t))
  ht-result)

;; To implement an e-class analysis, one should specialize E-GRAPH-E-CLASS-CLASS to return a custom class. Then, they should specialize MAKE-E-CLASS-FROM-SINGLE-E-NODE on that class to setup initial analysis data. Finally, they should implement E-GRAPH-E-CLASSES-MERGE to properly combine data from multiple e-classes.

;; general TODO: Probably should remove all specialization on e-classes; since both e-classes and e-graphs need to be parametrized based on the types of e-nodes, might as well centralize everything and only parametrize based on e-graphs.

(defclass e-graph ()
  ((e-node-e-classes :accessor e-graph-e-node-e-classes :type hash-table
                     :documentation "The \"hashcons\" from canonical e-nodes to their e-class IDs. After rebuilding, keys in this hashmap will always be canonical e-nodes, but the e-class-id values may or may not be canonical.")
   (e-classes :accessor e-graph-e-classes :type union-find
              :documentation "Union-find over e-class IDs.")
   (dirty-e-nodes :accessor e-graph-dirty-e-nodes :type hash-table
                  :documentation "Hash table queue of nodes that need to be re-canonicalized.")
   (e-node-car=-fn :initarg :e-node-car=-fn :initform #'equal :reader e-graph-e-node-car=-fn
                   :documentation "A function designator which compares e-node CARs. Defualt implementation is #'EQUAL. If not EQUAL, a custom hash function must also be provided.")
   (e-node-car-hash-fn :initarg :e-node-car-hash-fn :reader e-graph-e-node-car-hash-fn
                       :documentation "Function designator which hashes e-node CARs. Only used if a custom E-NODE-CAR=-FN is also provided.")
   (make-e-node-hash-table-fn :reader e-graph-make-e-node-hash-table-fn
                              :documentation "Constructs a hash table. Set automatically based on e-node CAR equality and hash functions, when provided."))
  (:documentation "Base e-graph class. Initialize an instance of this class to create a usual e-graph. Acceptable initargs are :E-NODE-CAR-TEST and :E-NODE-CAR-HASH-FUNCTION. If provided, the former shall be an equivalence relation and the latter a suitable hash function over the CARs of e-nodes that will be used in this e-graph. By default uses #'equal as the test. These initargs are only supported on CL implementations supporting custom hash table tests and hash functions, such as SBCL."))

(defgeneric e-graph-make-e-class-from-single-e-node (e-graph e-class-name en)
  (:documentation "Construct a new e-class with the given CLOS class name containing a single e-node")
  (:method ((eg e-graph) sym en)
    (declare (symbol sym) (e-node en))
    (make-instance sym :e-nodes (list en) :parents (funcall (e-graph-make-e-node-hash-table-fn eg)))))

(declaim (inline combine-hashes))
(defun combine-hashes (a b)
  ;; Shamelessly copied from Coalton
  (#+sbcl sb-int:mix
   #-sbcl logxor a b))

(declftype (function) function car=-fn->e-node=-fn)
(defun car=-fn->e-node=-fn (car=-fn)
  (lambda (a b)
    (and (funcall car=-fn (car a) (car b))
         (every #'= (cdr a) (cdr b)))))

(declftype (e-graph) function e-graph-e-node=-fn)
(defun e-graph-e-node=-fn (eg)
  (car=-fn->e-node=-fn (e-graph-e-node-car=-fn eg)))

(defgeneric e-graph-compute-make-e-node-hash-table-fn (eg)
  (:documentation "Return a function designator which creates empty hash tables which can store e-nodes as keys. Default implementation uses E-NODE-CAR=-FN and E-NODE-CAR-HASH-FN as one would expect.")
  (:method ((eg e-graph))
    (let ((car=-fn (e-graph-e-node-car=-fn eg)))
      (cond
        ((member car=-fn (list 'equal #'equal))
         (lambda ()
           (make-hash-table :test 'equal)))
        (t
         (let ((test-fn (car=-fn->e-node=-fn car=-fn))
               (hash-fn (e-graph-e-node-car-hash-fn eg)))
           (lambda ()
             (make-hash-table
              :test test-fn
              :hash-function (lambda (a)
                               (combine-hashes (funcall hash-fn (car a))
                                               (sxhash (cdr a))))))))))))

(defgeneric e-graph-e-classes-merge (eg ec1 ec2)
  (:documentation "Non-destructively merge the e-classes ec1 and ec2, returning a combined e-class.")
  (:method ((eg e-graph) (ec1 e-class) (ec2 e-class))
    (make-instance 'e-class
                   :e-nodes (append (e-class-e-nodes ec1) (e-class-e-nodes ec2))
                   :parents (hash-table-union (e-class-parents ec1) (e-class-parents ec2)
                                              (funcall (e-graph-make-e-node-hash-table-fn eg))))))

(defmethod initialize-instance :after ((eg e-graph) &key)
  (setf (e-graph-e-classes eg) (make-union-find :merge-fn (curry #'e-graph-e-classes-merge eg)))
  (let ((make-hash-table-fn (e-graph-compute-make-e-node-hash-table-fn eg)))
    (setf (e-graph-e-node-e-classes eg) (funcall make-hash-table-fn))
    (setf (e-graph-dirty-e-nodes eg) (funcall make-hash-table-fn))
    (setf (slot-value eg 'make-e-node-hash-table-fn) make-hash-table-fn)))

(defgeneric e-graph-e-class-class (eg)
  (:documentation "Return the CLOS class that e-classes of this e-graph should be instances of.")
  (:method ((eg e-graph))
    'e-class))

(declftype (e-graph) e-class-id e-graph-count-e-classes)
(defun e-graph-count-e-classes (eg)
  (union-find-count (e-graph-e-classes eg)))

(declftype (e-graph e-class-id) e-class-id e-graph-canonical-e-class-id)
(defun e-graph-canonical-e-class-id (eg ecid)
  (union-find-root (e-graph-e-classes eg) ecid))

(declftype (e-graph e-node) e-node e-graph-canonical-e-node)
(defun e-graph-canonical-e-node (eg en)
  ;; simply canonicalize e-class ids of every cdr element
  (cons (car en) (mapcar (curry #'e-graph-canonical-e-class-id eg) (cdr en))))

(declftype (e-graph e-node) (or null e-class-id) e-graph-e-node->e-class-id)
(defun e-graph-e-node->e-class-id (eg en)
  "Find the e-class for the given e-node, canonicalizing it on the way, or nil if none is found."
  (gethash (e-graph-canonical-e-node eg en) (e-graph-e-node-e-classes eg)))

(declftype (e-graph generalized-e-node) e-class-id e-graph-add)
(defun e-graph-add (eg en)
  "Add the given e-node to the e-graph if it is not already present. Regardless of whether or not it is present, return an e-class ID that contains the e-node. If the e-node is not already present, requires rebuild.

If EN is in fact an e-class ID, simply returns it.

If EN's children are not listed just as e-class IDs, but as full e-node forms, such e-nodes are constructed and inserted into EG and their e-class IDs are put in place of their forms. For instance, EN may be (+ 23 (2) (- 5 (1)))."
  (cond
    ((e-class-id-p en)
     en)
    (t
     (let* ((resolved-en (cons (car en)
                               (loop :for child :in (cdr en)
                                     :collect (etypecase child
                                                (cons (e-graph-add eg child))
                                                (e-class-id child)))))
            (canonical-en (e-graph-canonical-e-node eg resolved-en)))
       (or (e-graph-e-node->e-class-id eg canonical-en)
           (progn
             (let* ((new-e-class (e-graph-make-e-class-from-single-e-node eg (e-graph-e-class-class eg) canonical-en))
                    (new-id (union-find-add (e-graph-e-classes eg) new-e-class)))
               ;; install ourselves into hashcons
               (setf (gethash (copy-list canonical-en) (e-graph-e-node-e-classes eg)) new-id)
               ;; install ourselves as a parent of all our children
               (dolist (child (cdr canonical-en))
                 (hash-table-push (e-class-parents (union-find-get (e-graph-e-classes eg) child))
                                  canonical-en))
               new-id)))))))

(declftype (generalized-e-node) e-graph e-node->e-graph)
(defun e-node->e-graph (en)
  "Convenience function to make a new e-graph containing only the given generalized e-node."
  (let ((eg (make-instance 'e-graph)))
    (e-graph-add eg en)
    eg))

(declftype (e-graph e-class-id) e-class e-graph-e-class-id->e-class)
(defun e-graph-e-class-id->e-class (eg ecid)
  (union-find-get (e-graph-e-classes eg) ecid))

(declftype (e-graph &rest e-class-id) boolean e-graph-e-class-id=)
(defun e-graph-e-class-id= (eg &rest ecids)
  "Determine whether all the passed e-class IDs are equal"
  (or (null ecids)
      (loop :for ecid :in ecids
            :always (union-find-same-group-p (e-graph-e-classes eg) ecid (car ecids)))))

(declftype (e-graph e-class-id) list e-graph-mark-e-class-dirty)
(defun e-graph-mark-e-class-dirty (eg ecid)
  "Mark all parents of the e-class identified by ECID as dirty."
  (loop :for parent :being :each :hash-key :of (e-class-parents (e-graph-e-class-id->e-class eg ecid))
        :do (hash-table-push (e-graph-dirty-e-nodes eg) parent)))

(declftype (e-graph e-class-id e-class-id) t e-graph-merge)
(defun e-graph-merge (eg ecid1 ecid2)
  "Merge two e-classes together, by ID. All e-nodes in the two e-classes will be equivalent after this operation. If the two e-class IDs do not already refer to the same e-class, requires rebuild. Returns non-nil if the e-graph was actually modified (ie, the ecids didn't already point to the same e-class)."
  (let ((ec1 (e-graph-e-class-id->e-class eg ecid1))
        (ec2 (e-graph-e-class-id->e-class eg ecid2)))
    (when (not (eq ec1 ec2))
      (multiple-value-bind (preserved-ecid destroyed-ecid)
          (union-find-merge (e-graph-e-classes eg) ecid1 ecid2)
        (declare (ignore preserved-ecid))
        (e-graph-mark-e-class-dirty eg destroyed-ecid))
      t)))

(declftype (e-graph) boolean e-graph-rebuild)
(defun e-graph-rebuild (eg)
  "Rebuilds an egraph after possibly multiple writes. Returns non-nil if we actually did anything (ie, if the graph was dirty)."
  (with-accessors ((dirty-ens e-graph-dirty-e-nodes)
                   (hashcons e-graph-e-node-e-classes))
      eg
    (prog1
        (not (zerop (hash-table-count dirty-ens)))
      (loop :with e-node=-fn := (e-graph-e-node=-fn eg)
            :for dirty-en := (hash-table-pop dirty-ens)
            :while dirty-en
            :for old-children := (copy-list (cdr dirty-en))
            ;; TODO lots of room for performance improvements in the next few lines, for example only removing as a parent from those children which we no longer have.
            :for canonical-dirty-en := (e-graph-canonical-e-node eg dirty-en)
            :do (when (not (funcall e-node=-fn dirty-en canonical-dirty-en))
                  (let ((old-ecid (gethash dirty-en hashcons))
                        (new-ecid (gethash canonical-dirty-en hashcons)))
                    ;; Remove from the hashcons
                    (remhash dirty-en hashcons)
                    ;; Remove as parent of children
                    (dolist (child (cdr dirty-en))
                      (remhash dirty-en (e-class-parents (e-graph-e-class-id->e-class eg child))))
                    ;; Remove from e-class. TODO: Could be faster if change E-CLASS-E-NODES to be a hashmap.
                    (removef (e-class-e-nodes (e-graph-e-class-id->e-class eg old-ecid)) dirty-en)
                    (cond
                      (new-ecid
                       ;; Canonical boi is already in hash table, stand down
                       ;; Now dirty-en is completely erased. Time to merge
                       (e-graph-merge eg old-ecid new-ecid))
                      (t
                       ;; The canonical boi is not in the hash table yet. Assume control by remaining in the e-class and simply becoming canonical.
                       ;; Reinstall as parent.
                       (dolist (child (cdr dirty-en))
                         (hash-table-push (e-class-parents (e-graph-e-class-id->e-class eg child))
                                          canonical-dirty-en))
                       ;; Re-insert into e-class
                       ;; Perhaps surprisingly, it is not necessary to update the E-CLASS-E-NODES to be canonical. However, it might simplify or speed up other people's code and is the predictable thing to do.
                       (push canonical-dirty-en (e-class-e-nodes (e-graph-e-class-id->e-class eg old-ecid)))
                       ;; And reinsert into hashcons
                       (setf (gethash canonical-dirty-en hashcons) old-ecid)))))))))

(defclass pattern ()
  ())
(defclass naive-pattern (pattern)
  ((compiled-tree :initarg :compiled-tree :reader naive-pattern-compiled-tree
                  :initform (error "compiled-tree required") :type list)
   (num-vars :initarg :num-vars :reader naive-pattern-num-vars :initform (error "num-vars required"))))

(deftype pattern-binding ()
  "A variable binding, as the result of a pattern matching."
  'list)

(deftype maybe-binding ()
  '(or list (eql unbound)))

;; TODO: list of X type
(declftype (list list) pattern make-pattern)
(defun make-naive-pattern (pattern return-vars)
  "Create a pattern which will be matched naively against every e-node in the e-graph.

A pattern is an s-expression. Symbols which are not the CAR of a list are treated as variables to be bound. Underscores (in the CLUCK package) are wildcards. Symbols which are the CAR of a list are matched literally, with one exception: If the symbol is QUOTE, then there must be exactly one other item in the list, which the whole list is matched literally against. This is useful to exactly match a symbol that would normally be bound as a variable.

Example patterns:

'(+ a b) matches '(+ 5 2), '(+ x y), '(+ (* a b) 5)
'(+ 'a b) matches '(+ a 5), '(+ a b), but NOT '(+ 5 2) or '(+ b a)
'(+ _ _) matches the same things as '(+ a b) but does not return any bindings"
  (labels ((compile-tree (tree)
             (cond
               ((and (symbolp tree)
                     (member tree return-vars))
                `(bind ,(position tree return-vars)))
               ((eq '_ tree)
                '(wildcard))
               ((consp tree)
                (assert (symbolp (car tree)) () "Functions must be constants in current impl")
                `(application ,(car tree) ,@(mapcar #'compile-tree (cdr tree))))
               (t ; equivalent to (atom tree)
                (when (and (symbolp tree)
                           (string= "_" tree))
                  (warn "Tried to compile a pattern involving a symbol named \"_\" in a package other than CLUCK. This will not be treated as a wildcard!"))
                `(constant ,tree)))))
    (make-instance 'naive-pattern :compiled-tree (compile-tree pattern)
                                  :num-vars (length return-vars))))

(declftype (e-graph) list e-graph-e-class-id-list)
(defun e-graph-e-class-id-list (eg)
  "Return list of all distinct e-class IDs"
  (union-find-all (e-graph-e-classes eg)))

(declftype (e-graph) list e-graph-e-node-list)
(defun e-graph-e-node-list (eg)
  "Return list of all distinct e-nodes"
  (loop :for ecid :in (e-graph-e-class-id-list eg)
        :append (e-class-e-nodes (e-graph-e-class-id->e-class eg ecid))))

(declftype (list &key (:test (or symbol function))) list remove-duplicates-fast)
(defun remove-duplicates-fast (list &key test)
  "Same behavior as REMOVE-DUPLICATES, but uses a hash table instead of pairwise comparisons. As a consequence, the TEST parameter must be a valid hash table test."
  (loop :with ht := (make-hash-table :test test)
        :for item :in list
        :when (not (hash-table-member-p ht item))
          :do (hash-table-push ht item)
          :and :collect item))

(defgeneric e-graph-match (e-graph pattern)
  (:documentation "Find matches of PATTERN in E-GRAPH. Returns a list of matches, each one being of the form (E-CLASS-ID X-1 X-2 X-3 ...), where E-CLASS-ID denotes the e-class where the root node of the pattern matches, and X-1, X-2, etc are the bindings as defined when creating the pattern."))

(defmethod e-graph-match ((eg e-graph) (pattern naive-pattern))
  (let ((car=-fn (e-graph-e-node-car=-fn eg)))
    (labels ((match-node (ecid pat-tree partial-bindings)
               "Attempt to match the pat-tree against this specific e-class, returning a list of bindings"
               (let ((ec (e-graph-e-class-id->e-class eg ecid)))
                 (ecase (car pat-tree)
                   (bind
                       (set-bindings-nth partial-bindings (cadr pat-tree) ecid))
                   (wildcard
                    partial-bindings)
                   (constant
                    ;; search for an e-node matching the constant
                    (when (member (cdr pat-tree) (e-class-e-nodes ec) :test car=-fn)
                      partial-bindings))
                   (application
                    (loop :for en :in (e-class-e-nodes ec)
                          :when (and (eql (car en) (cadr pat-tree))         ; function symbols agree
                                     (= (length en) (1- (length pat-tree)))) ; same arity
                            :append (loop :for child-ecid :in (cdr en)
                                          :for child-pat :in (cddr pat-tree)
                                          ;; we want to AND across the bindings, ie, find a binding that is compatible with our pattern across all the different parameters.
                                          :for bindings := (match-node child-ecid child-pat partial-bindings)
                                            :then (match-node child-ecid child-pat bindings)
                                          :finally (return bindings)))))))
             (set-binding-nth (binding n val)
               "Attempt to set the N-th value of BINDING to VAL. Returns an updated binding or the symbol UNBOUND if not possible. Does not mutate the BINDING parameter."
               (let* ((binding (copy-list binding))
                      (the-cons (nthcdr n binding)))
                 (cond
                   ((or (eq 'unbound (car the-cons))
                        (= (car the-cons) val))
                    (rplaca the-cons val)
                    binding)
                   (t
                    'unbound))))
             (set-bindings-nth (bindings n val)
               "Attempt to set the N-th value of each binding in BINDINGS to VAL. Any incompatible bindings are removed."
               (loop :for binding :in bindings
                     :for updated-binding := (set-binding-nth binding n val)
                     :when (not (eq 'unbound updated-binding))
                       :collect updated-binding)))
      (declare (ftype (function (e-class-id t list) list) match-node)
               (ftype (function (pattern-binding fixnum t) maybe-binding) set-binding-nth)
               (ftype (function (list fixnum t) list) set-bindings-nth))
      (loop :for ecid :in (e-graph-e-class-id-list eg)
            :append (mapcar (curry #'cons ecid)
                            (remove-duplicates-fast ; TODO: determine if we ever actually use this
                             (match-node ecid
                                         (naive-pattern-compiled-tree pattern)
                                         (list (make-list (naive-pattern-num-vars pattern)
                                                          :initial-element 'unbound)))
                             :test car=-fn))))))

(defun default-car-match-fn (a b)
  (if (eql a b)
      nil
      'no-match))

(defun default-merge-car-bound-values-fn (a b id)
  (declare (ignore id))
  (if (eql a b)
      (values a t)
      nil))

(defclass graph-pattern (pattern)
  ((binding-restrictions
    :initarg :binding-restrictions
    :documentation "Hashmap from binding symbols/other identifiers to e-node-like lists. The car of each of those lists is an object suitable to be the first arguent to CAR-MATCH-FN, while the rest of the list should be EQL-able objects which refer either to unbound bindings or to other entries in BINDING-RESTRICTIONS")
   (car-match-fn
    :initarg :car-match-fn :initform #'default-car-match-fn
    :type (or symbol (function (t t) (or list (eql 'no-match))))
    :documentation "A function which takes the CAR of a matcher from BINDING-RESTRICTIONS and the CAR of an e-node, and returns either an alist of bindings or the symbol NO-MATCH.

The default implementation succeeds with no bindings if the arguments are EQL, and fails to match otherwise.")
   (merge-car-bound-values-fn
    :initarg :merge-car-bound-values-fn :initform #'default-merge-car-bound-values-fn
    :type (or symbol (function (t t t) (values t boolean)))
    :documentation "A function which takes two possibly conflicting values assigned to the same binding id by CAR-MATCH-FN, and either reconciles them into a single value which it returns along with a second return value T, or fails to do so and returns (values nil nil). The last argument is the binding id the values were assigned to. It is often ignored.

The default implementation merges if the arguments are EQL, else fails.")
   (%binding-order :initform nil
    :documentation "The order in which the bindings will be matched. Set automatically")
   (%binding-parents
    :initform (make-hash-table)
    :documentation "Hashmap from binding IDs to a list of bindings listing this one as a child. Set automatically"))
  (:documentation "A pattern which may be a graph rather than just a tree. For example, this allows us to match a pattern where two separate nodes must have the same child, but also enforce some restrictions on that child. It also allows matching graphs with multiple roots. Finally, it allows custom CAR matchers that allow parametrization based on the CAR of an e-node.

All binding symbols which have restrictions (ie, appear as keys in BINDING-RESTRICTIONS) will be bound to specific e-nodes in the result. Binding symbols without restrictions (ie, appear as children in some element(s) of BINDING-RESTRICTIONS but do not appear as keys in BINDING-RESTRICTIONS) will be bound to e-class IDs, which implicitly represent that they could be bound to any node in the designated e-class.

Note that the graph of the pattern must be connected."))

(defmethod initialize-instance :after ((pattern graph-pattern) &key)
  ;; Algorithm to determine %binding-order:
  ;; 1. Choose an arbitrary starting node.
  ;; 2. Go forwards and backwards recursively, not stepping on the same stone twice.
  ;; TODO: optimize: Our backwards matching gives us tons of flexibility about the order, and we can probably speed things up a /lot/ by matching in the optimal order.
  (with-slots (binding-restrictions %binding-order %binding-parents) pattern
    (flet ((parents (binding-id)
             "Return binding identifiers which have this one as children"
             (loop :for other-binding-id :being :each :hash-key :of binding-restrictions
                     :using (:hash-value other-binding-value)
                   :when (member binding-id (cdr other-binding-value))
                     :collect other-binding-id)))
      (let ((stack (list (hash-table-peek binding-restrictions))))
        (loop :while stack
              :for elt := (pop stack)
              :do (when (not (member elt %binding-order))
                    (push elt %binding-order)
                    ;; only want children with restrictions
                    (let ((children (loop :for child :in (cdr (gethash elt binding-restrictions))
                                          :when (gethash child binding-restrictions)
                                            :collect child))
                          ;; parents are naturally all in binding-restrictions
                          (parents (parents elt)))
                      (setf stack (append children parents stack))
                      (setf (gethash elt %binding-parents) parents)))
              :finally (progn
                         (assert (set-equal %binding-order (hash-table-keys binding-restrictions))
                                 () "Attempted to instantiate a graph pattern where not all gates are connected. (Not yet implemented, and would be slow if implemented)")
                         (setf %binding-order (reverse %binding-order))))))))

(declftype (graph-pattern e-graph list) t check-graph-binding)
(defun check-graph-binding (pat eg bindings-list)
  "Ensure that all of the bindings obey the constraints set out in the pattern."
  (with-slots (binding-restrictions car-match-fn merge-car-bound-values-fn) pat
    (dolist (bindings bindings-list)
      (loop :for binding-id :being :each :hash-key :of binding-restrictions
              :using (:hash-value restriction)
            :for bound-cons := (assoc binding-id bindings)
            :for bound := (progn
                            (assert bound-cons () "Binding id ~A was restricted but not in bindings." binding-id)
                            (cdr bound-cons))
            :for unchecked-car-match := (funcall car-match-fn (car restriction) (car bound))
            :for car-match := (progn
                                (assert (not (eql 'no-match unchecked-car-match)) ()
                                        "Binding id ~A car failed to match (bound car was ~A)" binding-id (car bound))
                                unchecked-car-match)
            ;; check that the car match is compatible with bindings
            :do (loop :for (c-binding-id . c-bound) :in car-match
                      :do (assert (assoc c-binding-id bindings) ()
                                  "Car of id ~A matched binding-id ~A not found in bindings." binding-id c-binding-id)
                      :do (multiple-value-bind (_ merged?)
                              (funcall merge-car-bound-values-fn
                                       c-bound (assoc-value bindings c-binding-id)
                                       c-binding-id)
                            (declare (ignore _))
                            (assert merged? (c-bound (assoc-value c-binding-id bindings))
                                    "Car matched binding value for id ~A incompatible with what was found in bindings." c-binding-id)))
                ;; check that the binding corresponds to an actual e-node
            :do (assert (e-graph-e-node->e-class-id eg bound) (bound)
                        "Matched e-node for binding-id ~A not actually present in the e-graph." binding-id)
                ;; check that the children belong to the e-classes they should.
            :do (loop :for child-ecid :of-type e-class-id :in (cdr bound)
                      :for child-binding-id :in (cdr restriction)
                      :for child-bound := (assoc-value bindings child-binding-id)
                      :do (assert (or (eql child-ecid child-bound) ; if unrestricted, will be ecid
                                      (e-graph-e-class-id= eg
                                                           child-ecid
                                                           (e-graph-e-node->e-class-id eg child-bound)))
                                  () "A child of binding id ~A, specifically ~A, did not agree with the ecid bound in the parent." binding-id child-binding-id))))))

(defmethod e-graph-match ((eg e-graph) (pattern graph-pattern))
  (with-slots (binding-restrictions car-match-fn merge-car-bound-values-fn %binding-order %binding-parents) pattern
    (let (result
          (make-e-node-hash-table-fn (e-graph-make-e-node-hash-table-fn eg)))
      (labels ((make-e-node-hash-table ()
                 (funcall make-e-node-hash-table-fn))

               (expand-generalized-e-node-set (set)
                 "A generalized e-node set is either nil (representing unrestricted), a list of e-nodes, an e-class ID, or a hash table with e-nodes as keys. The present function converts to the hash table representation."
                 (etypecase set
                   (hash-table set)
                   (list (list->hash-table set (make-e-node-hash-table)))
                   (e-class-id
                    ;; We could cache the hash table per e-node here, but testing indicates not much improvement.
                    (list->hash-table (e-class-e-nodes (e-graph-e-class-id->e-class eg set)) (make-e-node-hash-table)))))

               (generalized-e-node-set-intersection (set1 set2)
                 (cond
                   ((null set1)
                    set2)
                   ((null set2)
                    set1)
                   ((and (e-class-id-p set1) (e-class-id-p set2))
                    (if (= set1 set2)
                        set1
                        'empty))
                   (t
                    (hash-table-intersection (expand-generalized-e-node-set set1)
                                             (expand-generalized-e-node-set set2)
                                             (make-e-node-hash-table)))))

               (generalized-e-node-set-singleton-get (set)
                 "Given a generalized e-node set containing only a single e-node, return said e-node."
                 (etypecase set
                   (list
                    (assert (= 1 (length set)))
                    (car set))
                   (hash-table
                    (assert (= 1 (hash-table-count set)))
                    (hash-table-peek set))))

               (generalized-e-node-set-empty-p (set)
                 ;; TODO: consider empty e-class if we allow removing from the e-graph in the future?
                 (or (eql 'empty set)
                     (and (hash-table-p set)
                          (zerop (hash-table-count set)))))

               (match-single-node (binding-id bindings-so-far en)
                 "Returns either a list of updated bindings or the symbol NO-MATCH to indicate a matching failure."
                 (let* ((restrictions (gethash binding-id binding-restrictions))
                        (_ (assert restrictions () "MATCH-SINGLE-NODE only work on restricted bindings"))
                        ;; First match the restriction CAR against the e-node CAR
                        (car-match-bindings (funcall car-match-fn (car restrictions) (car en))))
                   (declare (ignore _))

                   (when (or (eql 'no-match car-match-bindings)
                             ;; possible optimization: Perform this check before doing car matching to help fail faster.
                             (/= (length restrictions) (length en)))
                     (return-from match-single-node 'no-match))
                   (loop :for (binding-id . value) :in car-match-bindings ; Not quite correct: if the alist returned from car-match-bindings has duplicates, because we'll process them twice. This isn't a critical problem, for most reasonable merge functions.
                         :for old-binding := (assoc binding-id bindings-so-far)
                         :when old-binding
                           :do (multiple-value-bind (new-value compatible-p)
                                   (funcall merge-car-bound-values-fn value (cdr old-binding) binding-id)
                                 (if compatible-p
                                     (push (cons binding-id new-value) bindings-so-far)
                                     (return-from match-single-node 'no-match)))
                         :else
                           :do (push (cons binding-id value) bindings-so-far))

                   ;; check children
                   (loop :for child-binding-id :in (cdr restrictions)
                         :for child-ecid :in (cdr en)
                         :for intersection
                           := (generalized-e-node-set-intersection child-ecid (assoc-value bindings-so-far child-binding-id))
                         :do (when (generalized-e-node-set-empty-p intersection)
                               (return-from match-single-node 'no-match))
                         :do (push (cons child-binding-id intersection) bindings-so-far))
                   ;; and now parents. TODO: deduplicate with children case
                   (loop :with parent-ens
                           := (e-class-parents
                               (e-graph-e-class-id->e-class
                                eg (e-graph-e-node->e-class-id eg en)))
                         :for parent-binding-id :in (gethash binding-id %binding-parents)
                         :for intersection
                           := (generalized-e-node-set-intersection parent-ens (assoc-value bindings-so-far parent-binding-id))
                         :do (when (generalized-e-node-set-empty-p intersection)
                               (return-from match-single-node 'no-match))
                         :do (push (cons parent-binding-id intersection) bindings-so-far))
                   ;; If we made it this far the binding was successful.
                   ;; Caller should already have checked that this en is compatible.
                   `((,binding-id . (,en)) ,@bindings-so-far)))

               (match (binding-order-remaining bindings-so-far)
                 (let* ((current-binding-id (car binding-order-remaining))
                        (rest-binding-ids (cdr binding-order-remaining)))
                   (cond
                     ;; Every binding already matched, we're outta here!
                     ((null binding-order-remaining) ; everything has been matched
                      (push bindings-so-far result))
                     (t
                      ;; If our traversal order is valid, should have a candidate list
                      (let ((candidate-ens (assoc-value bindings-so-far current-binding-id)))
                        (assert candidate-ens () "Attempted to match a node but it didn't have a list of candidates available. %BINDING-ORDER is probably malformed.")
                        (loop :for candidate-en :being :each :hash-key
                                :of (expand-generalized-e-node-set candidate-ens)
                              :for updated-bindings
                                := (match-single-node current-binding-id bindings-so-far candidate-en)
                              :when (not (eql 'no-match updated-bindings))
                                :do (match rest-binding-ids updated-bindings))))))))
        
        ;; initial bindings: Bind the first node to be bound to list of all nodes, leave others nil (implicitly unrestricted, cf generalized e-node set intersection).
        (match %binding-order `((,(car %binding-order) . ,(e-graph-e-node-list eg))))
        ;; All the bindings which correspond to e-nodes should be narrowed down to single e-nodes by now.
        (let ((final-result
                (loop :for bindings :in result
                      :for clean-bindings := (remove-duplicates bindings :key #'car :from-end t)
                      :collect (loop :for (binding-id . val) :in clean-bindings
                                     :when (hash-table-member-p binding-restrictions binding-id)
                                       :collect (cons binding-id
                                                      (generalized-e-node-set-singleton-get val))
                                     :else
                                       :collect (cons binding-id val)))))
          (check-graph-binding pattern eg final-result)
          final-result)))))

(defvar *all-rewriters* (make-hash-table))

(deftype rewrite-replacement ()
  "Either a function, which performs the rewrite and returns a boolean indicating whether it mutated the graph, or an e-class-id and a generalized e-node to merge it with."
  '(or (function () t)
    (cons e-class-id list)))

(deftype rewrite-replacements ()
  "List of REWRITE-REPLACEMENT type objects"
  'list)

(declftype (e-graph rewrite-replacements) t e-graph-apply-rewrite-replacements)
(defun e-graph-apply-rewrite-replacements (eg replacements)
  "Apply the replacements to the e-graph in-place. Returns non-nil if the e-graph was actually modified"
  (let (modified-p)
    (dolist (replacement replacements)
      (declare (rewrite-replacement replacement))
      (etypecase replacement
        (function
         (when (funcall replacement)
           (setf modified-p t)))
        (list
         (dolist (en (cdr replacement))
           (declare (generalized-e-node en))
           (when (e-graph-merge eg (car replacement) (e-graph-add eg en))
             (setf modified-p t))))))
    modified-p))

(defclass rewriter ()
  ((name :initarg :name :reader rewriter-name :type symbol)
   (tags :initarg :tags :accessor rewriter-tags :type list
         :documentation "List of EQL-able tags which might be used to enable or disable certain rewriters.")
   (rewrite-fn :initarg :rewrite-fn :initform (error "rewrite-fn required")
               ;; SBCL doesn't want to check this type at runtime.
               ;:type (function (e-graph) rewrite-replacements)
               :documentation "Performs matching and returns list of rewrites")))

(defmethod print-object ((rewriter rewriter) stream)
  (print-unreadable-object (rewriter stream :type t)
    (format stream "~a" (rewriter-name rewriter))))

(declftype ((or rewriter symbol) &key (:error boolean)) (or rewriter null) get-rewriter)
(defun get-rewriter (rewriter &key (error t))
  "Get the named rewriter. Throws an error if not found an ERROR is non-nil."
  (etypecase rewriter
    (rewriter rewriter)
    (symbol (let ((result (gethash rewriter *all-rewriters*)))
              (when (and error (not result))
                (error "Rewriter ~a not found." rewriter))
              (values (gethash rewriter *all-rewriters*))))))

(declftype (&optional (or package string-designator)) list get-package-rewriters)
(defun get-package-rewriters (&optional (package *package*))
  "Return a list of all registered rewriters whose names lie in the given package."
  (mapcar 'get-rewriter
          (remove-if-not (compose (curry #'eq package)
                                  #'symbol-package)
                         (hash-table-keys *all-rewriters*))))

(declftype (&optional (or package string-designator)) t delete-package-rewriters)
(defun delete-package-rewriters (&optional (package *package*))
  (loop :with hitlist
        :for key :of-type symbol :being :each :hash-key :of *all-rewriters*
        :when (eq (symbol-package key) package)
          :do (push key hitlist)
        :finally (dolist (hit hitlist)
                   (remhash hit *all-rewriters*))))

(defmethod rewriter-get-replacements ((rewriter rewriter) eg)
  (declare (e-graph eg))
  (funcall (slot-value rewriter 'rewrite-fn) eg))

(declftype (e-graph (or rewriter symbol) &key (:rebuild t)) t e-graph-apply-rewriter)
(defun e-graph-apply-rewriter (eg rewriter &key (rebuild t))
  "Apply a single rewriter to the e-graph, and optionally rebuild afterwards. This is mainly a function for interactive/debugging use; when applying multiple rewrite rules, it's best to apply them in bulk to avoid extraneous rebuilds."
  (e-graph-apply-rewrite-replacements eg (rewriter-get-replacements (get-rewriter rewriter) eg))
  (when rebuild
    (e-graph-rebuild eg)))

(defmacro make-rewrite-fn (pattern return-vars &body body)
  "Create a function suitable for the REWRITE-FN slot in a REWRITER object. During execution of BODY, the RETURN-VARS will be bound to whatever they matched in the pattern. The BODY should return a list of e-nodes that are equivalent to the matched e-node.

PATTERN is evaluated, RETURN-VARS is not."
  (declare (list return-vars))
  (once-only (pattern)
    (with-gensyms (eg matched-ecid)
      `(progn
         (check-type ,pattern pattern)
         (lambda (,eg)
           (declare (e-graph ,eg))
           (loop :for (,matched-ecid ,@return-vars) :in (e-graph-match ,eg ,pattern)
                 :collect (cons ,matched-ecid (progn ,@body))))))))

(defmacro make-naive-rewrite-fn (pattern return-vars &body body)
  "Construct a function suitable for the REWRITE-FN slot in a REWRITER object. Unlike MAKE-REWRITE-FN, this function also constructs a naive pattern, passing PATTERN and RETURN-VARS as arguments to it."
  (declare (list return-vars))
  `(make-rewrite-fn (make-naive-pattern ,pattern ',return-vars) ,return-vars ,@body))

(defun register-rewriter (name rewriter)
  (declare (symbol name) (rewriter rewriter))
  (when (hash-table-member-p *all-rewriters* name)
    (warn "Redefining rewriter ~a" name))
  (setf (gethash name *all-rewriters*)
        rewriter))

(defmacro define-naive-rewriter (name pattern return-vars &body body)
  "Construct a REWRITER object using a naive rewriter."
  (declare (symbol name))
  `(register-rewriter ',name (make-instance 'rewriter
                                            :name ',name
                                            :rewrite-fn (make-naive-rewrite-fn ,pattern ,return-vars ,@body))))

;;;; EQUALITY SATURATION

(declftype ((or list (eql :all)) &optional list list) list filter-rewriters-by-tags)
(defun filter-rewriters-by-tags (include-tags &optional exclude-tags
                                                (all-rewriters (hash-table-keys *all-rewriters*)))
  "Return a subset of all-rewriters containing only rewriters whose tag lists intersect with INCLUDE-TAGS and do not intersect with EXCLUDE-TAGS. INCLUDE-TAGS may be the keyword symbol :ALL."
  (loop :for rewriter-designator :in all-rewriters
        :for rewriter := (get-rewriter rewriter-designator)
        :when (and (or (eql :all include-tags)
                       (remove-if-not (rcurry #'member include-tags) (rewriter-tags rewriter)))
                   (not (remove-if-not (rcurry #'member (rewriter-tags rewriter)) exclude-tags)))
          :collect rewriter))

(declftype (e-graph list &key (:noise (or boolean stream)) (:max-repetitions (integer 1)) (:summary-noise (or boolean stream))) t e-graph-equality-saturate-naive)
(defun e-graph-equality-saturate-naive (eg rewriters &key noise (max-repetitions 50) (summary-noise noise))
  "Run the rules in REWRITERS until no more apply, or unil MAX-REPETITIONS is reached. Each element of REWRITERS can be a REWRITER object or a symbol designating one. Unless the rewriters are designed carefully to terminate, this function will usually reach the max repetitions. If NOISE is non-nil, it is used as the first argument to FORMAT and successfully applied rewrite rules are printed there."
  (loop :with rewriters := (mapcar 'get-rewriter rewriters)
        :for i :from 0 :below max-repetitions
        :for rewriter-replacements :=
        #+lparallel
         (lparallel:pmap 'list
                         (lambda (rewriter)
                           (format noise "Matching ~A~%" (rewriter-name rewriter))
                           (cons rewriter (rewriter-get-replacements rewriter eg)))
                         rewriters)
        #-lparallel
         (loop :for rewriter :in rewriters
               :for replacements := (progn
                                      (format noise "Matching ~a~%" (rewriter-name rewriter))
                                      (rewriter-get-replacements rewriter eg))
               :collect (cons rewriter replacements))
        :for progress := nil
        :do (loop :for (rewriter . replacements) :in rewriter-replacements
                  :do (when (e-graph-apply-rewrite-replacements eg replacements)
                        (format noise "Rewrote with ~A (~A)~%" (rewriter-name rewriter) (length replacements))
                        (setf progress t)))
        :while progress
        :do (format summary-noise "Finished rewriting for iteration ~a, rebuilding.~%" i)
        :do (e-graph-rebuild eg)
        :do (when summary-noise
              (format summary-noise "Current e-graph size: ~a ECs, ~a ENs~%"
                      (e-graph-count-e-classes eg)
                      (length (e-graph-e-node-list eg))))))

;;;; ANALYSIS

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

;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; DEBUGGING TOOLS ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;

(declftype (e-graph) t check-e-graph-hashcons-invariant)
(defun check-e-graph-hashcons-invariant (eg)
  "Throw an error if the hashcons invariant does not hold"
  ;; For each e-node, make sure its canonical version is in the hashcons and points to the right place.
  ;; In the end, check that there are no extraneous entries in the hashcons
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
                                       (gethash canonical-e-node hashcons)))) ; avoid re-canonicalizing e-node
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

;; TODO: add a function to print all representations of an e-class. Will be a bit annoying to deal with circular graphs.

(declftype (function cons) string prin1-e-node-to-string)
(defun prin1-e-node-to-string (to-printable en)
  (prin1-to-string (cons (funcall to-printable (car en))
                         (cdr en))))

(declftype (e-graph &key (:stream (or stream (eql t))) (:e-node-car-to-printable function)) t e-graph-print)
(defun e-graph-print (eg &key (stream t) (e-node-car-to-printable #'identity))
  (let ((num-dirty-ens (hash-table-count (e-graph-dirty-e-nodes eg)))
        (ids (e-graph-e-class-id-list eg)))
    (when (not (zerop num-dirty-ens))
      (warn "WARNING: The e-graph is dirty! (~a many dirty nodes).~%~%" num-dirty-ens))
    (loop :for id :in ids
          :for ec := (e-graph-e-class-id->e-class eg id)
          :do (progn
                (format stream "+---------------+~%| E-CLASS ~a~%+---------------+~%" id)
                ;; I know format supports loops, don't @ me
                (loop :for e-node :in (e-class-e-nodes ec)
                      :do (format stream "| ~a~%"
                                  (prin1-e-node-to-string e-node-car-to-printable e-node)))
                (format stream "+---------------+~%~%")))))

(defvar *dot-node-attrs* ""
  "A string of extra attributes to apply to nodes (e-nodes) in graphviz plots.")
(defvar *dot-subgraph-attrs* ""
  "A string of extra attributes to apply to subgraphs (e-classes) in graphviz plots.")
(defvar *dot-layout-engine* "dot")

(declftype (e-graph &key (:stream (or stream (eql t))) (:e-node-car-to-printable function)) t e-graph-plot-dot)
(defun e-graph-plot-dot (eg &key (stream t) (e-node-car-to-printable #'identity))
  "Plot the e-graph as a DOT file suitable as input to graphviz."
  (let ((num-dirty-ens (hash-table-count (e-graph-dirty-e-nodes eg)))
        (ids (e-graph-e-class-id-list eg)))
    (when (not (zerop num-dirty-ens))
      (warn "WARNING: The e-graph is dirty! (~a many dirty nodes).~%~%" num-dirty-ens))
    (format stream "digraph {~%compound = true; ~% node [shape=rectangle style=rounded ~a] graph [style=dashed color=\"#999999\" ~a]~%"
            *dot-node-attrs* *dot-subgraph-attrs*)
    ;; Add subgraphs and e-nodes
    (loop :for id :in ids
          :for ec := (e-graph-e-class-id->e-class eg id)
          :do (progn
                ;; starting the name of the subgraph with "cluster" is required for it to visually group the nodes together.
                (format stream "subgraph cluster~s {~% " id)
                ;; I know format supports loops, don't @ me
                (loop :for e-node :in (e-class-e-nodes ec)
                      :do (format stream "~s [label=~s];~%"
                                  (prin1-e-node-to-string e-node-car-to-printable e-node)
                                  (prin1-to-string (funcall e-node-car-to-printable (car e-node)))))
                (format stream "}~%")))
    ;; Add edges
    (loop :for id :in ids
          :for ec := (e-graph-e-class-id->e-class eg id)
          :do (loop :for en :in (e-class-e-nodes ec)
                    :do (loop :for child :in (cdr en)
                              :for i :from 0
                              ;; The DOT language requires all edges to connect nodes, not subgraphs. So we must find an arbitrary element of the target e-class, and then use the lhead attribute to make the edge visually point to the subgraph.
                              ;; Also unfortunately, graphviz doesn't let you do self-edges from a cluster to itself (or nodes contained in the cluster to the cluster), at least not without hacks like invisible nodes, so we just do a nodewise self-edge where appropriate.
                              :for child-en
                                := (if (= id child)
                                       en
                                       (car (e-class-e-nodes (e-graph-e-class-id->e-class eg child))))
                              :do (assert (= child (e-graph-canonical-e-class-id eg child)))
                              :do (format stream "~s -> ~s [lhead = cluster~s ~@[label=\" ~a\"~]];~%"
                                          (prin1-e-node-to-string e-node-car-to-printable en)
                                          (prin1-e-node-to-string e-node-car-to-printable child-en)
                                          child
                                          ;; when multiple children, label edges
                                          (when (< 1 (length (cdr en)))
                                            i)))))
    (format stream "}~%")))

(declftype (e-graph &rest t) t e-graph-plot-gui)
(defun e-graph-plot-gui (eg &rest opts)
  "Plot the given e-graph to svg using graphviz and open the result in the default application. To customize this behavior, use E-GRAPH-PLOT-DOT instead."
  (let ((dotstr (with-output-to-string (stream)
                  (apply 'e-graph-plot-dot eg :stream stream opts))))
    (uiop:with-temporary-file (:pathname pathname :type "svg" :keep t)
      (with-input-from-string (stream dotstr)
        (uiop:run-program `(,*dot-layout-engine* "-Tsvg" "-o" ,(namestring pathname))
                          :input stream))
      (uiop:run-program `("open" ,(namestring pathname))))))
