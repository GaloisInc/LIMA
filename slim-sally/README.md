# Atom to Sally Translator

[![Build Status](https://travis-ci.org/GaloisInc/atom-sally.svg?branch=master)](https://travis-ci.org/GaloisInc/atom-sally)

This is a library for translating Atom [1] [1b] specifications into transition
systems in the model language of Sally [2].

This is a work in progress, please check back soon!


# Quick Start

Run the `install.sh` script located in the top-level directory of the
repository. This will download our forked version of the Atom DSL, and build
the Atom to Sally Translator in place. See "Less Quick Start" below for more
on generating and model-checking models.


# Less Quick Start

You'll need our fork of Atom and a recent Haskell development environment.
You'll also need a working copy of Sally if you want to check the generated
models.

We don't package a build script (yet), but the following commands, in order,
will get you up and running (note that the `install.sh` script already performs
a few of these actions)::

```
$ git clone git@github.com:GaloisInc/atom
$ git clone git@github.com:GaloisInc/atom-sally
$ cd atom-sally
$ cabal sandbox init
$ cabal sandbox add-source ../atom
$ cabal configure --enable-tests
$ cabal install --only-dependencies --enable-tests
$ cabal build
$ cabal test --show-details=always
```

The last command will take the three example Atom specs from `test/Spec.hs` and
compile them to Sally input files `test/*.mcmt`.

To use Sally to decide the queries listed in `test/Spec.hs` you can use
something like the following:

```
$ sally --engine=ic3 test/A1.mcmt
```

# References

[1] https://github.com/tomahawkins/atom

[1b] https://github.com/GaloisInc/atom (version of atom that is compatible
     with this library)

[2] https://github.com/SRI-CSL/sally
