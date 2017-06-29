#!/bin/bash

git submodule update

if [ -d .cabal-sandbox ]; then
  echo "Skipping sandbox build..."
else
  cabal sandbox init
  cabal sandbox add-source ../../lima/
  cabal sandbox add-source ../../lima-c/
  cabal sandbox add-source ../../lima-sally/
  cabal sandbox add-source ../../dependencies/language-sally
  cabal install --only-dependencies
fi

cabal configure
cabal build
cabal install
