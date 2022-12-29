#!/bin/bash

version=2802fb3f784ef8d1e18235890c1b16bc41ea9bab

set -e -o pipefail

TMP="$(mktemp -d)"
trap "rm -rf $TMP" EXIT

rm -rf opam-file-format
mkdir -p opam-file-format/src

(
    cd $TMP
    git clone https://github.com/ocaml/opam-file-format.git
    cd opam-file-format
    git checkout $version
)

SRC=$TMP/opam-file-format

cp -v $SRC/src/*.{ml,mli,mll,mly} opam-file-format/src

git checkout opam-file-format/src/dune
git add -A .
