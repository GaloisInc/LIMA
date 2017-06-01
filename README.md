SLIM
====

[![Build Status](https://travis-ci.org/GaloisInc/SLIM.svg?branch=master)](https://travis-ci.org/GaloisInc/SLIM)

(S)pecification (L)anguage for (I)ntegrated (M)odeling

Contents
--------

* `slim`: Core SLIM language library
* `slim-c`: C code generator for SLIM
* `slim-sally`: Sally model generator for SLIM
* `case-studies`: Case study systems expressed in SLIM

The `slim` and `slim-c` packages are direct forks of `Atom`, originally due to
Tom Hawkins and Lee Pike.


Quick Start
-----------

To generate code and models from the case studies, first make sure you have
installed the Haskell compiler `GHC`, version 7.10 or above and `cabal-install`
version 1.24 or above. Using your system's package manager is the best way to
achieve this.

Then change to the case study's directory and use `cabal-install` to build
SLIM and the example:

```
$ cd case-studies/OM1
$ cabal new-configure
$ cabal new-build
```

The executable which generates code and models is given on the last line of
output, e.g.

```
Linking /Users/alice/slim/case-studies/OM1/dist-newstyle/build/slim-om1-0.2.0.0/build/slim-om1/slim-om1
```

