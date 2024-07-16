;;;; Author: Mark Polyakov, released under MIT License

(uiop:define-package :CLuck
  (:use :cl
        :alexandria)
  (:export
   :e-graph
   :e-class
   :e-node
   :e-class-id
   :e-node-p
   :e-node-atom
   :e-node-atom-p
   :e-class-e-nodes
   :e-graph-rebuild
   :e-graph-add
   :e-graph-merge
   :e-graph-match
   :e-graph-count-e-classes
   :e-graph-e-node->e-class-id
   :e-graph-e-class-id->e-class
   :e-graph-e-class-id=
   :e-graph-canonical-e-class-id
   :e-graph-e-node-car=-fn
   :e-graph-e-node-car-hash-fn
   :e-graph-make-e-node-hash-table-fn
   :e-graph-e-class-id-list
   :e-graph-e-node-list
   :check-e-graph

   :e-graph-print
   :*dot-layout-engine*
   :*dot-node-attrs*
   :*dot-subgraph-attrs*
   :e-graph-plot-dot
   :e-graph-plot-gui

   :make-naive-pattern
   :_
   :rewriter
   :rewriter-tags
   :get-rewriter
   :get-package-rewriters
   :delete-package-rewriters
   :make-rewriter
   :make-naive-rewriter
   :define-naive-rewriter
   :register-rewriter
   :filter-rewriters-by-tags
   :e-graph-apply-rewriter
   :*all-rewriters*

   :graph-pattern
   :no-match

   :e-graph-equality-saturate-naive

   :e-analyzer
   :e-analyzer-cost
   :e-analyzer-e-node-value
   :e-analyzer-compare
   :e-analyzer-merge
   :e-analyzer-finalize
   :e-analyzer-parent-value

   :+tree-size-analyzer+
   :+tree-depth-analyzer+

   :e-graph-cheapest-representation-by-analyzer))
