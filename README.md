LIMA
====

[![Build Status](https://travis-ci.org/GaloisInc/LIMA.svg?branch=master)](https://travis-ci.org/GaloisInc/LIMA)

(L)anguage for (I)ntegrated (M)odeling and (A)nalysis


Overview
--------

LIMA is a domain specific language developed under NASA contract NNL14AA08. It
is designed as a language for generating implementations, formal models, and
architectural models from a common specification. In the area of
fault-tolerant distributed systems, formal models are often produces from a
specification using ad-hoc abstractions. These abstracts tend to cause
implementations, and formals models to diverge. Having a common specification
source from which we automatically generate implementation and formal model
alleviates this.


Contents
--------

* `lima`: Core LIMA language library
* `lima-c`: C code generator for LIMA
* `lima-sally`: Sally model generator for LIMA
* `case-studies`: Case study systems expressed in LIMA

The `lima` and `lima-c` packages are direct forks of `Atom`, originally due to
Tom Hawkins and Lee Pike.


Quick Start
-----------

To generate code and models from the case studies, first make sure you have
installed the Haskell compiler `GHC`, version 7.10 or above and `cabal-install`
version 1.24 or above. Using your system's package manager is the best way to
achieve this.

Then change to the case study's directory and use `cabal-install` to build
LIMA and the example:

```
$ cd case-studies/OM1
$ cabal new-configure
$ cabal new-build
```

The executable which generates code and models is given on the last line of
output, e.g.

```
Linking /Users/alice/lima/case-studies/OM1/dist-newstyle/build/lima-om1-0.2.0.0/build/lima-om1/lima-om1
```

