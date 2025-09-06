#!/bin/bash

version=e85bc867f03f5b253bf0335b829edd1e55b0e8c0

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
