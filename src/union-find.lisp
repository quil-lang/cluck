;;;; Author: Mark Polyakov, released under MIT License

(in-package :cluck)

(deftype union-find-id ()
  "An integer type large enough to hold the size of any reasonable union-find"
  (if (>= most-positive-fixnum (expt 2 60))
      'fixnum
      `(integer 0 ,(expt 2 60))))

(defun first-arg (a b)
  (declare (ignore b))
  a)

(defstruct (union-find)
  (parents (make-array '(0) :element-type 'union-find-id :adjustable t :fill-pointer t) :type (vector union-find-id) :read-only t)
  (sizes (make-array '(0) :element-type 'union-find-id :adjustable t :fill-pointer t) :type (vector union-find-id) :read-only t)
  (data-hash (make-hash-table) :type hash-table :read-only t)
  (merge-fn #'first-arg :type function))

(declftype (union-find) union-find-id union-find--size)
(defun union-find--size (union-find)
  (length (union-find-parents union-find)))

(declftype (union-find union-find-id) union-find-id union-find-root)
(defun union-find-root (union-find x)
  "Find the root of X (can be thought of as a canonical ID), and flatten the tree along the way."
  ;; TODO: is there some way to only perform assertions at high safety? (Or just detect the safety optimization level)
  (assert (< x (union-find--size union-find)))
  (with-accessors ((parents union-find-parents)) union-find
    (loop :for cur := x :then parent
          :for parent := (elt parents cur) :then grandparent
          :for grandparent := (elt parents parent)
          :while (/= cur parent)
          :do (setf (elt parents cur) grandparent)
          :finally (return parent))))

(declftype (union-find union-find-id union-find-id) boolean union-find-same-group-p)
(defun union-find-same-group-p (union-find a b)
  (= (union-find-root union-find a) (union-find-root union-find b)))

(declftype (union-find union-find-id) (values t &optional) union-find-get)
(defun union-find-get (union-find x)
  "Retrieve the data corresponding to ID X. SETF-able. Second return value indicates whether "
  (values (gethash (union-find-root union-find x) (union-find-data-hash union-find))))

(declftype (t union-find union-find-id) t (setf union-find-get))
(defun (setf union-find-get) (d union-find x)
  (setf (gethash (union-find-root union-find x) (union-find-data-hash union-find)) d)
  d)

(declftype (union-find union-find-id union-find-id) union-find-id union-find-merge)
(defun union-find-merge (union-find a b)
  "Merge together the nodes whose IDs are A and B. The data of the merged node will be the result of calling the union-find's MERGE-FN with the data of A as the first argument and the data of B as the second argument. Returns two values: The ID of the root node that a and b both belong to now, and the root node of the tree that got merged into that one. (If a and b are already root nodes, returns either (values a b) or (values b a))"
  (with-accessors ((parents union-find-parents) (sizes union-find-sizes) (data union-find-data-hash))
      union-find
    (let* ((a-root (union-find-root union-find a))
           (b-root (union-find-root union-find b))
           (a-root-size (elt sizes a-root))
           (b-root-size (elt sizes b-root))
           (a-root-data (gethash a-root data))
           (b-root-data (gethash b-root data))
           (a-smol-p (< a-root-size b-root-size))
           (smol-root (if a-smol-p a-root b-root))
           (big-root (if a-smol-p b-root a-root)))
      (remhash a-root data)
      (remhash b-root data)
      (setf (gethash big-root data) (funcall (union-find-merge-fn union-find) a-root-data b-root-data))
      (setf (elt parents smol-root) big-root)
      (setf (elt sizes big-root) (+ a-root-size b-root-size))
      (values big-root smol-root))))

(declftype (union-find t) union-find-id union-find-add)
(defun union-find-add (union-find d)
  "Insert a new element with data D. Returns the integer ID of this new element."
  (let ((new-id (union-find--size union-find)))
    (vector-push-extend new-id (union-find-parents union-find))
    (vector-push-extend 1 (union-find-sizes union-find))
    (setf (union-find-get union-find new-id) d)
    new-id))

(declftype (union-find (function (union-find-id) t)) t union-find-map)
(defun union-find-map (union-find fn)
  "Run FN on every canonical/root ID in the UNION-FIND"
  (loop :for i :from 0
        :for parent :across (union-find-parents union-find)
        :when (= i parent)
          :do (funcall fn i)))

(declftype (union-find) list union-find-all)
(defun union-find-all (union-find)
  "Return a list of all canonical/root IDs in the UNION-FIND"
  (let (result)
    (union-find-map union-find (lambda (i) (push i result)))
    result))

(declftype (union-find) union-find-id union-find-count)
(defun union-find-count (union-find)
  "The number of elements in the union-find"
  (loop :for i :from 0
        :for parent :across (union-find-parents union-find)
        :when (= i parent)
          :sum 1))
