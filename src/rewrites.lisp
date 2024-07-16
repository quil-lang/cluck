;;;; Author: Mark Polyakov, released under MIT License

(in-package #:cluck)

(defvar *rewrite-rules* (make-hash-table))

(defvar *all-rewriters* (make-hash-table))

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
                (and (symbolp tree)
                     (string= "_" tree)
                     (warn "Tried to compile a pattern involving a symbol named \"_\" in a package other than CLUCK. This will not be treated as a wildcard!"))
                `(constant ,tree)))))
    (make-instance 'naive-pattern :compiled-tree (compile-tree pattern)
                                  :num-vars (length return-vars))))

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
  (unless (eql a b)
    'no-match))

(defun default-merge-car-bound-values-fn (a b id)
  (declare (ignore id))
  (when (eql a b)
    (values a t)))

(defclass graph-pattern (pattern)
  ((binding-restrictions
    :initarg :binding-restrictions
    :documentation "Hashmap from binding symbols/other identifiers to e-node-like
lists. The car of each of those lists is an object suitable to be the
first arguent to CAR-MATCH-FN, while the rest of the list should be
EQL-able objects which refer either to unbound bindings or to other
entries in BINDING-RESTRICTIONS")
   (car-match-fn
    :initarg :car-match-fn
    :initform #'default-car-match-fn
    :type (or symbol (function (t t) (or list (eql 'no-match))))
    :documentation "A function which takes the CAR of a matcher from BINDING-RESTRICTIONS
and the CAR of an e-node, and returns either an alist of bindings or
the symbol NO-MATCH.

The default implementation succeeds with no bindings if the arguments
are EQL, and fails to match otherwise.")
   (merge-car-bound-values-fn
    :initarg :merge-car-bound-values-fn
    :initform #'default-merge-car-bound-values-fn
    :type (or symbol (function (t t t) (values t boolean)))
    :documentation "A function which takes two possibly conflicting values assigned to the
same binding id by CAR-MATCH-FN, and either reconciles them into a
single value which it returns along with a second return value T, or
fails to do so and returns (values nil nil). The last argument is the
binding id the values were assigned to. It is often ignored.

The default implementation merges if the arguments are EQL, else fails.")
   (%binding-order :initform nil
                   :documentation "The order in which the bindings will be matched. Set automatically")
   (%binding-parents
    :initform (make-hash-table)
    :documentation "Hashmap from binding IDs to a list of bindings listing this one as a
child. Set automatically"))
  (:documentation "A pattern which may be a graph rather than just a tree. For example,
this allows us to match a pattern where two separate nodes must have
the same child, but also enforce some restrictions on that child. It
also allows matching graphs with multiple roots. Finally, it allows
custom CAR matchers that allow parametrization based on the CAR of an
e-node.

All binding symbols which have restrictions (ie, appear as keys in
BINDING-RESTRICTIONS) will be bound to specific e-nodes in the
result. Binding symbols without restrictions (ie, appear as children
in some element(s) of BINDING-RESTRICTIONS but do not appear as keys
in BINDING-RESTRICTIONS) will be bound to e-class IDs, which
implicitly represent that they could be bound to any node in the
designated e-class.

Note that the graph of the pattern must be connected."))

(defmethod initialize-instance :after ((pattern graph-pattern) &key)
  ;; Algorithm to determine %binding-order:
  ;; 1. Choose an arbitrary starting node.
  ;; 2. Go forwards and backwards recursively, not stepping on the
  ;; same stone twice.
  ;; TODO: optimize: Our backwards matching gives us tons of
  ;; flexibility about the order, and we can probably speed things up
  ;; a /lot/ by matching in the optimal order.
  (with-slots (binding-restrictions %binding-order %binding-parents) pattern
    (flet ((parents (binding-id)
             "Return binding identifiers which have this one as children"
             (loop
               :for other-binding-id :being :each :hash-key :of binding-restrictions
                 :using (:hash-value other-binding-value)
               :when (member binding-id (cdr other-binding-value))
                 :collect other-binding-id)))
      (loop
        :with stack = (list (hash-table-peek binding-restrictions))
        :while stack
        :for elt := (pop stack)
        :unless (member elt %binding-order)
          :do (push elt %binding-order)
              ;; only want children with restrictions
              (let ((children
                      (loop
                        :for child :in (cdr (gethash elt binding-restrictions))
                        :when (gethash child binding-restrictions)
                          :collect child))
                    ;; parents are naturally all in binding-restrictions
                    (parents (parents elt)))
                (setf stack (append children parents stack))
                (setf (gethash elt %binding-parents) parents))
        :finally (assert
                  (set-equal %binding-order (hash-table-keys binding-restrictions))
                  ()
                  "Attempted to instantiate a graph pattern where not all gates are
connected. (Not yet implemented, and would be slow if implemented)")
                 (setf %binding-order (reverse %binding-order))))))

(declftype (graph-pattern e-graph list) t check-graph-binding)
(defun check-graph-binding (pat eg bindings-list)
  "Ensure that all of the bindings obey the constraints set out in the pattern."
  (with-slots (binding-restrictions car-match-fn merge-car-bound-values-fn) pat
    (dolist (bindings bindings-list)
      (loop
        :for binding-id :being :each :hash-key :of binding-restrictions
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
