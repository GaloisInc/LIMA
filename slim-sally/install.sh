#!/bin/bash

# setup deps directory for non-hackage dependencies
if [ ! -d deps ]; then
    echo "Creating deps/ ..."
    mkdir deps
fi

# update atom
cd deps
if [ ! -d atom ]; then
    echo "Cloning atom..."
    git clone https://github.com/GaloisInc/atom
else
    echo "Pulling atom..."
    cd atom; git pull; cd ..
fi
cd ..

if [ ! -d .cabal-sandbox ]; then
    echo "Building new sandbox..."
    cabal sandbox init
    cabal sandbox add-source deps/atom
    cabal install --only-dependencies --enable-tests
fi

echo "Building..."
cabal build
echo "Testing..."
cabal test --show-details=always
