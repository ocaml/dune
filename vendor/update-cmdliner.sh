#!/bin/bash

version=54089df3de0045845bb8a19177bc588c9044e0ac

set -e -o pipefail

TMP="$(mktemp -d)"
trap "rm -rf $TMP" EXIT

rm -rf cmdliner
mkdir -p cmdliner/src

(
    cd $TMP
    git clone https://github.com/ElectreAAS/dune-cmdliner.git
    mv dune-cmdliner cmdliner
    cd cmdliner
    git checkout $version
)

SRC=$TMP/cmdliner

cp -v $SRC/LICENSE.md cmdliner
cp -v $SRC/src/*.{ml,mli} cmdliner/src

git checkout cmdliner/src/dune
git add -A .
