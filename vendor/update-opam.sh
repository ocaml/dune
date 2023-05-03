#!/bin/bash

version=34ed40d0dcab0e5c6a613e29422e18f6473d7bcc

set -e -o pipefail

TMP="$(mktemp -d)"
trap "rm -rf $TMP" EXIT

rm -rf opam
mkdir -p opam/src

(
    cd $TMP
    git clone https://github.com/ocaml-dune/opam.git
    cd opam
    git checkout $version
)

SRC=$TMP/opam

for subpackage in core repository format
do
    PKG=opam/src/$subpackage/
    mkdir -p $PKG
    set +e
    cp -v $SRC/src/$subpackage/*.{ml,mli,mll,mly} $PKG
    set -e
    cp -v $SRC/src/$subpackage/dune $PKG
    git checkout $PKG/dune
done

git add -A opam/
