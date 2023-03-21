#!/bin/bash

version=8474b2a29c4cb9cbf356006612c7b2b97e9c4087

set -e -o pipefail

TMP="$(mktemp -d)"
trap "rm -rf $TMP" EXIT

PACKAGE=uutf

rm -rf $PACKAGE
mkdir -p $PACKAGE/src

(
    cd $TMP
    git clone https://github.com/dbuenzli/$PACKAGE.git
    cd $PACKAGE
    git checkout $version
    cd src
)

SRC=$TMP/$PACKAGE

cp -v $SRC/LICENSE.md $PACKAGE
cp -v $SRC/src/*.{ml,mli} $PACKAGE/

git checkout $PACKAGE/dune
git add -A .
