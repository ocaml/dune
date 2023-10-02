#!/bin/bash

<<<<<<< HEAD
version=e184beb298d9abe0c524a4839bb0bec3d2571282
=======
version=48a7145ca41e60d7124e6215bb19139b846985be
>>>>>>> parent of be9aa2820 (feature: use posix_spawn on macos (#8090))

set -e -o pipefail

TMP="$(mktemp -d)"
trap "rm -rf $TMP" EXIT

rm -rf spawn
mkdir -p spawn/src

(
    cd $TMP
    git clone https://github.com/ocaml-dune/spawn.git
    cd spawn
    git checkout $version
)

SRC=$TMP/spawn

cp -v $SRC/src/spawn{.ml,.mli,_stubs.c} spawn/src
cp -v $SRC/LICENSE.md spawn/

git checkout spawn/src/dune
git add -A .
