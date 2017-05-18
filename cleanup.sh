#!/bin/bash
for f in $(find -E . -regex '.*dist-newstyle|.*dist|.*cabal\.project\.local');
do
    echo "Removing $f"
    rm -rf "$f"
done
