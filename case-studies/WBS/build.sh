#!/bin/bash

if [ -d .cabal-sandbox ]; then
  echo "Skipping sandbox build..."
else
  cabal sandbox init
  cabal sandbox add-source ../../slim/
  cabal sandbox add-source ../../slim-c/
  cabal sandbox add-source ../../slim-sally/
  cabal install --only-dependencies
fi

cabal configure
cabal build
cabal install
