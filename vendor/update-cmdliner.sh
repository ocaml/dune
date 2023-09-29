#!/bin/bash

version=430664dd13c4645bda0e9c39f7d0ec75c1534874

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
