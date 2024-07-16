(in-package :cluck)

(defmacro declftype (params ret &rest fnames)
  `(declaim (ftype (function ,params ,ret) ,@fnames)))

(declftype (hash-table) t hash-table-peek)
(defun hash-table-peek (ht)
  (with-hash-table-iterator (next ht)
    (multiple-value-bind (has-value-p k) (next)
      (if has-value-p
          (values k t)
          (values nil nil)))))

(declftype (hash-table) t hash-table-pop)
(defun hash-table-pop (ht)
  "Remove and return an arbitrary key of the hash table HT, or NIL if none are present. The second returned value indicates whether anything was actually removed from the hash table (ie, if HT was originally nonempty)"
  (multiple-value-bind (k has-value-p) (hash-table-peek ht)
    (when has-value-p
      (remhash k ht))
    (values k has-value-p)))

;; The age old problem of push argument order. List first, or obj first? If you put obj first, you
;; make it harder to curry by violating the rule of thumb of putting the least frequently changing
;; arguments first. If you put the list first, you break the standard that's (unfortunately) already
;; been set by CL:PUSH. However, Emacs' ADD-TO-LIST agrees with me, so CL:PUSH is outvoted 2-to-1.
(declftype (hash-table t) t hash-table-push)
(defun hash-table-push (ht k)
  "Add a key to the hash table ht with T value. Useful when treating a hash table as an unordered queue with unique elements."
  (setf (gethash k ht) t))

(declftype (list hash-table) hash-table list->hash-table)
(defun list->hash-table (l ht-result)
  "Take every element of the list and insert it as a key into ht-result."
  (dolist (i l)
    (hash-table-push ht-result i))
  ht-result)

(declftype (hash-table hash-table &optional hash-table) hash-table hash-table-intersection)
(defun hash-table-intersection (ht1 ht2
                                &optional (ht-result (make-hash-table :test (hash-table-test ht1))))
  "Return a hash table whose keys are the intersection of the keys in HT1 and HT2. Cf HASH-TABLE-UNION."
  (assert (eql (hash-table-test ht1) (hash-table-test ht2)) () "Arguments to HASH-TABLE-INTERSECTION must use the same test.")
  (flet ((add (ht1 ht2)
           (loop :for k :being :each :hash-key :of ht1
                 :do (when (gethash k ht2)
                       (setf (gethash k ht-result) t)))))
    (add ht1 ht2)
    (add ht2 ht1)
    ht-result))

(declftype (hash-table t) boolean hash-table-member-p)
(defun hash-table-member-p (ht k)
  (nth-value 1 (gethash k ht)))
