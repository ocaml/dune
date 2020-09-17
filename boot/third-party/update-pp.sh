#!/bin/bash

version=5b54adcb46c9e110d2cfcce96a3e812566c437fc

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

git add -A .
