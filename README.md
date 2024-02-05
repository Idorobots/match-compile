# match-compile
Figuring out how to compile `match` expressions in a simple Lisp dialect.

So far the following methods are implemented:
- Naïve sequential match,
- Path-combining algorithm (inspired by [Compiling Pattern Matching to Good Decision Trees](http://moscova.inria.fr/~maranget/papers/ml05e-maranget.pdf))


A benchmark is provided in `test/benchmark.rkt` comparing the performance of the various methods against the Scheme implementation used:

```
$ racket test/benchmark.rkt
Racket match:  cpu time: 633 real time: 635 gc time: 346
Naïve match:   cpu time: 5106 real time: 5121 gc time: 495
Combine match: cpu time: 1710 real time: 1714 gc time: 411
```
