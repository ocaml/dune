#!/bin/bash

version=bcf425c65184eef6a8306f33b76c935e67620c77

set -e -o pipefail

TMP="$(mktemp -d)"
trap "rm -rf $TMP" EXIT

rm -rf csexp
mkdir -p csexp/src

(
    cd $TMP
    git clone https://github.com/diml/csexp.git
    cd csexp
    git checkout $version
)

SRC=$TMP/csexp

cp -v $SRC/src/csexp.{ml,mli} csexp/src

git checkout csexp/src/dune
git add -A .
