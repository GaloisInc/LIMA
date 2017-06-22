#!/bin/bash

CASE_STUDIES="OM1 WBS periodic"

git submodule update --init

for c in $CASE_STUDIES; do
  pushd "case-studies/$c"
  cabal new-configure
  cabal new-build
  popd
done
