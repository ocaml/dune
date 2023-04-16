#!/bin/bash

version=e6c4768e10c61bcb04d09748744dad55602149c6

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
