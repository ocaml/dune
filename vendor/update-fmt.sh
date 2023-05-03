#!/usr/bin/env bash

version=4175d4cc6bb2a99b93e993cdb47e43fc8d27acfa

set -e -o pipefail

TMP="$(mktemp -d)"
trap "rm -rf $TMP" EXIT

PACKAGE=fmt

rm -rf $PACKAGE
mkdir -p $PACKAGE/src

(
    cd $TMP
    git clone https://github.com/ocaml-dune/$PACKAGE.git
    cd $PACKAGE
    git checkout $version
)

SRC=$TMP/$PACKAGE

cp -v $SRC/LICENSE.md $PACKAGE/
cp -v -R $SRC/src/fmt.ml{i,} $PACKAGE/src/

git checkout $PACKAGE/src/dune
git add -A .
