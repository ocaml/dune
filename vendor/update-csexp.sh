#!/bin/bash

version=07eb8988452ad51a09d0ab7379d73a87674aba6e

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
cp -v $SRC/LICENSE.md csexp/

git checkout csexp/src/dune
git add -A .
