LIMA
====

[![Build Status](https://travis-ci.org/GaloisInc/LIMA.svg?branch=master)](https://travis-ci.org/GaloisInc/LIMA)

(S)pecification (L)anguage for (I)ntegrated (M)odeling

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

