#!/bin/bash

version=c7f97d02cedc3d7e267704b987f3c1403e8152a9

set -e -o pipefail

TMP="$(mktemp -d)"
trap "rm -rf $TMP" EXIT

rm -rf cmdliner
mkdir -p cmdliner/src

(
    cd $TMP
    git clone https://github.com/ocaml-dune/cmdliner.git
    cd cmdliner
    git checkout $version
    dune subst
    cd src
)

SRC=$TMP/cmdliner

cp -v $SRC/LICENSE.md cmdliner
cp -v $SRC/src/*.{ml,mli} cmdliner/src

git checkout cmdliner/src/dune
git add -A .
