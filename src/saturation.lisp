;;;; Author: Mark Polyakov, released under MIT License

(in-package :cluck)

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
