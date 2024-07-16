;;;; Computer Algebra System using the CLuck e-graph library
;;;; Copyright (c) Mark Polyakov, released under MIT license

(in-package #:cluck/examples/cas)

;;;; REWRITE RULES

;; Commutativity and Associativity:

(define-naive-rewriter commute-add '(+ a b) (a b) `((+ ,b ,a)))
(define-naive-rewriter commute-multiply '(* a b) (a b) `((* ,b ,a)))
(define-naive-rewriter associate-add-1 '(+ (+ a b) c) (a b c) `((+ ,a (+ ,b ,c))))
(define-naive-rewriter associate-add-2 '(+ a (+ b c)) (a b c) `((+ (+ ,a ,b) ,c)))
(define-naive-rewriter associate-multiply-1 '(* (* a b) c) (a b c) `((* ,a (* ,b ,c))))
(define-naive-rewriter associate-multiply-2 '(* a (* b c)) (a b c) `((* (* ,a ,b) ,c)))

;; Distributivity:

(define-naive-rewriter distribute-multiply-add-1 '(* a (+ b c)) (a b c) `((+ (* ,a ,b) (* ,a ,c))))
(define-naive-rewriter distribute-multiply-add-2 '(+ (* a b) (* a c)) (a b c) `((* ,a (+ ,b ,c))))

;; Cancellation:

(define-naive-rewriter cancel-add '(+ a (- a)) (a) '((0)))
(define-naive-rewriter cancel-multiply '(* a (/ a)) (a) '((1)))

;; Identity and Zero:
;; Zeroes can cause the e-graph to blow up, because once a and (+ 0 a) are unified, some other rules can be applied infinitely many times. One possible solution is to add an option to the pattern matcher to exclude matches involving loops.

(define-naive-rewriter identity-multiply '(* a 1) (a) `(,a))
(define-naive-rewriter identity-add '(+ a 0) (a) `(,a))
(define-naive-rewriter zero-multiply '(* _ 0) () '((0)))

(defparameter *cas-rewriters*
  '(commute-add commute-multiply
    associate-add-1 ; associate-add-2 associate-multiply-1 associate-multiply-2
    distribute-multiply-add-1 distribute-multiply-add-2
    cancel-add cancel-multiply
    identity-multiply identity-add zero-multiply))

(defun simplify (expr &key noise)
  "Simplify a mathematical expression such as (+ (* 2 3) 8)"
  (labels ((wrap-numbers (tree)
             (etypecase tree
               (atom (list tree))
               (cons (cons (car tree) (mapcar #'wrap-numbers (cdr tree))))))
           (unwrap-numbers (tree)
             (etypecase tree
               (e-node-atom
                (car tree))
               (cons ; every element of the tree should be a cons
                (cons (car tree) (mapcar #'unwrap-numbers (cdr tree)))))))
    (let* ((eg (make-instance 'e-graph))
           (root-ecid (e-graph-add eg (wrap-numbers expr))))
      (e-graph-equality-saturate-naive eg *cas-rewriters* :noise noise)
      (values
       (unwrap-numbers
        (e-graph-cheapest-representation-by-analyzer eg root-ecid +tree-size-analyzer+))
       eg))))

;; to use, run E-GRAPH-EQUALITY-SATURATE-NAIVE, then E-GRAPH-CHEAPEST-REPRESENTATION-BY-ANALYZER

;;;; E-CLASS ANALYSIS

;; (defparameter +float-comparison-threshold+ 0.00001)
;; (defparameter +greatest-nice-denominator+ 4
;;   "If the denominator of a fraction is greater than this, we don't consider it to be a \"nice\" number.")

;; (defun find-multiple (c x)
;;   "If x is a nice multiple of c, return the rounded x/c. Else, return nil."
;;   (declare (number c x))
;;   (let* ((unrounded (/ (* +greatest-nice-denominator+ x) c))
;;          (rounded (round unrounded)))
;;     (when (<= (abs (- rounded unrounded)) +float-comparison-threshold+)
;;       (/ rounded +greatest-nice-denominator+))))

;; (define-dynamic-naive-rewriter (multiple-pi)
;;     (a)
;;   (and (numberp a)
;;        (find-multiple pi a)))

;; (define-dynamic-naive-rewriter (multiple-e)
;;     (a)
;;   (and (numberp a)
;;        (find-multiple (exp 1) a)))

;; ;;
