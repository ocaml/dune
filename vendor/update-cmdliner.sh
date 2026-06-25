#!/bin/bash

version=bdd9c10e9b8a6fdd183d5733bcccb409e8a75731

set -e -o pipefail

TMP="$(mktemp -d)"
trap "rm -rf $TMP" EXIT

rm -rf cmdliner
mkdir -p cmdliner/src/tool

(
    cd $TMP
    git clone https://github.com/ocaml-dune/cmdliner.git
    cd cmdliner
    git checkout $version
)

SRC=$TMP/cmdliner

cp -v $SRC/LICENSE.md cmdliner
cp -v $SRC/src/*.{ml,mli} cmdliner/src
cp -v $SRC/src/tool/cmdliner_data.ml cmdliner/src/tool/cmdliner_data.ml

git checkout cmdliner/src/dune
git add -A .
