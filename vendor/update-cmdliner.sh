#!/bin/bash

version=b4f565634f63a67387c941a311236b2ae6d1aef2

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
