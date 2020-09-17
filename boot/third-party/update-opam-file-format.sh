#!/bin/bash

version=2.0.0

set -e -o pipefail

TMP="$(mktemp -d)"
trap "rm -rf $TMP" EXIT

rm -rf opam-file-format
mkdir -p opam-file-format/src

(
    cd $TMP
    git clone https://github.com/ocaml/opam-file-format
    cd opam-file-format
    git checkout $version
)

SRC=$TMP/opam-file-format

cp -v $SRC/src/*.{ml,mli,mll,mly} opam-file-format/src

git add -A .
