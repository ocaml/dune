#!/bin/bash

version=posix_spawn

set -e -o pipefail

TMP="$(mktemp -d)"
trap "rm -rf $TMP" EXIT

rm -rf spawn
mkdir -p spawn/src

(
    cd $TMP
    git clone https://github.com/rgrinberg/spawn.git
    cd spawn
    git checkout $version
)

SRC=$TMP/spawn

cp -v $SRC/src/spawn{.ml,.mli,_stubs.c} spawn/src
cp -v $SRC/LICENSE.md spawn/

git checkout spawn/src/dune
git add -A .
