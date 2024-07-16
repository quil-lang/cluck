;;;; Author: Mark Polyakov, released under MIT License

(in-package #:cluck)

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
