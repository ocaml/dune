#!/usr/bin/env bash

version=b58af5db6afd496cfd4a5f85fb23f30ba8dfbc87

set -e -o pipefail

TMP="$(mktemp -d)"
trap "rm -rf $TMP" EXIT

PACKAGE=0install-solver

rm -rf $PACKAGE
mkdir -p $PACKAGE/src

(
    cd $TMP
    git clone https://github.com/0install/0install.git
    cd 0install
    git checkout $version
)

SRC=$TMP/0install

cp -v $SRC/COPYING $PACKAGE/
cp -v -R $SRC/src/solver $PACKAGE/src/

git checkout $PACKAGE/src/solver/dune
rm -rv 0install-solver/src/solver/tests
git add -A .
