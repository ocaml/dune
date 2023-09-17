#!/usr/bin/env bash

version=b759d7c1c2f140724020f57599ead9ee394a8f7a

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
cp -v -R $SRC/lib $PACKAGE/

git checkout $PACKAGE/lib/dune
git add -A .
