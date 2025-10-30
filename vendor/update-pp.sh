#!/bin/sh

version=b6741dd41ef5fc5bda8b3640097ac29818a43577

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
cp -v $SRC/LICENSE.md pp/

git checkout pp/LICENSE.md
git add -A .
