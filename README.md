# match-compile
Figuring out how to compile `match` expressions in a simple Lisp dialect.

So far the following methods are implemented:
- Naïve sequential match,
- Path-combining algorithm (inspired by [Compiling Pattern Matching to Good Decision Trees](http://moscova.inria.fr/~maranget/papers/ml05e-maranget.pdf))


A benchmark is provided in `test/benchmark.scm` comparing the performance of the various methods against the Scheme implementation used.
