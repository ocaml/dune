#!/bin/bash

version=6be5e76d48806245255407b4d5881efe703cb5a0

set -e -o pipefail

TMP="$(mktemp -d)"
trap "rm -rf $TMP" EXIT

rm -rf pp
mkdir -p pp/src

(
    cd $TMP
    git clone https://github.com/ocaml-dune/pp.git
    cd pp
    git checkout $version
)

SRC=$TMP/pp

cp -v $SRC/src/pp.{ml,mli} pp/src

git checkout pp/src/dune
git add -A .
