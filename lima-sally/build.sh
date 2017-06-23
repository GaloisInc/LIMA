#!/bin/bash

cabal sandbox init
cabal sandbox add-source ../lima
cabal sandbox add-source ../dependencies/language-sally

cabal configure --enable-tests
cabal install --only-dependencies
cabal build

cabal test
