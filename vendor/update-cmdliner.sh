#!/bin/bash

version=ac44bb7d73d0f2e56b110099b529458d8302bf15

set -e -o pipefail

TMP="$(mktemp -d)"
trap "rm -rf $TMP" EXIT

rm -rf cmdliner
mkdir -p cmdliner/src

(
    cd $TMP
    git clone https://github.com/dbuenzli/cmdliner.git
    cd cmdliner
    dune subst
    cd src
    sed -i.bak 's/Pervasives.result/Dune_caml.result/g' *.{ml,mli}
)

SRC=$TMP/cmdliner

cp -v $SRC/LICENSE.md cmdliner
cp -v $SRC/src/*.{ml,mli} cmdliner/src

git checkout cmdliner/src/dune
git add -A .
