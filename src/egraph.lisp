;;;; Author: Mark Polyakov, released under MIT License

(in-package #:cluck)

;; An enode is represented as a list whose CAR is a symbol denoting a
;; function and whose CDR is a list of e-class IDs

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
    (unless (eq ec1 ec2)
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


(declftype (e-graph) list e-graph-e-class-id-list)
(defun e-graph-e-class-id-list (eg)
  "Return list of all distinct e-class IDs"
  (union-find-all (e-graph-e-classes eg)))

(declftype (e-graph) list e-graph-e-node-list)
(defun e-graph-e-node-list (eg)
  "Return list of all distinct e-nodes"
  (loop :for ecid :in (e-graph-e-class-id-list eg)
        :append (e-class-e-nodes (e-graph-e-class-id->e-class eg ecid))))
