#!/usr/bin/env bash

version=f4e5478242f9d70340b8d6e74a75ed035d4d544a

set -e -o pipefail

TMP="$(mktemp -d)"
trap "rm -rf $TMP" EXIT

PACKAGE=opam-0install

rm -rf $PACKAGE
mkdir -p $PACKAGE

(
    cd $TMP
    git clone https://github.com/ocaml-opam/opam-0install-solver.git
    cd opam-0install-solver
    git checkout $version
)

SRC=$TMP/opam-0install-solver

cp -v $SRC/LICENSE.md $PACKAGE/
cp -v -R $SRC/lib/{model,s,solver}.* $PACKAGE/

git checkout $PACKAGE/lib/dune
git add -A .
