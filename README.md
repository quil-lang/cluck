# CLuck
### _Mark Polyakov <mapolyakov@hrl.com>_

Equivalence Graph (e-graph) implementation for Common Lisp.

An e-graph is a data structure and set of algorithms for maintaining a set of
equivalent graphs, such as mathematical formulae representing the same function, or multiple
computer programs with the same behavior.

A typical application of e-graphs is to find the "best" element of a set of equivalent trees (eg,
the simplest formula, or the fastest computer program). Usually this is achieved by "equality
saturation", wherein rules are applied to repeatedly add equivalent items to the e-graph until the
rules fail to add any new items, at which point the graph is "saturated" and the best element
according to some cost function is chosen.

## Requirements

CLuck is written mainly in portable Common Lisp, but some optional features are not portable. For
example, specifying a custom `E-NODE-CAR-TEST` requires support for custom hash table tests and hash functions.

CLuck lists its required dependencies in its asd file, like any good Lisp project. `LPARALLEL` is an
optional dependency. If installed, CLuck will take advantage of it. (For example,
`E-GRAPH-EQUALITY-SATURATE-NAIVE` will match different rewrite rules in parallel (though it still
has to insert replacements in series)).

## License

MIT
