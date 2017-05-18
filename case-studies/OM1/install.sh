#!/bin/bash

set -eu

ATOMDIR=${ATOMDIR:-}
PROJ=$(basename $PWD)

if [ -z "$ATOMDIR" ]; then
    echo "Please specify location of 'atom' and 'atom-sally' base directory"
    echo "by setting ATOMDIR=..."
    exit 1
fi

echo "ATOMDIR = ${ATOMDIR}"
echo "PROJ    = ${PROJ}"

if [ "${1:-}" == "--clean" ]; then
    echo "Cleaning sandbox..."
    cabal sandbox delete || true
    rm -rf dist || true
fi

if [ ! -d .cabal-sandbox ]; then
    echo "Setting up sandbox..."
    cabal sandbox init
    cabal sandbox add-source "$ATOMDIR/atom"
    cabal sandbox add-source "$ATOMDIR/atom-sally"
    echo "Installing dependencies..."
    cabal install --only-dependencies
fi

echo "Installing ${PROJ}..."
cabal install

echo "Running $PROJ..."
cabal run
