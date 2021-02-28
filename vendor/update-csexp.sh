#!/bin/bash

version=0e1b2044c8d1ff187c27cec3e46d9cde14892650

set -e -o pipefail

TMP="$(mktemp -d)"
trap "rm -rf $TMP" EXIT

rm -rf csexp
mkdir -p csexp/src

(
    cd $TMP
    git clone https://github.com/ocaml-dune/csexp.git
    cd csexp
    git checkout $version
)

SRC=$TMP/csexp

cp -v $SRC/src/csexp.{ml,mli} csexp/src

git checkout csexp/src/dune
git add -A .
