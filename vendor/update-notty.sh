#!/bin/bash

version=67c602e3b32fdd6f4356c8e6b7537ff811ac14bf

set -e -o pipefail

TMP="$(mktemp -d)"
trap "rm -rf $TMP" EXIT

rm -rf notty
mkdir -p notty/src notty_unix

(
    cd $TMP
    git clone https://github.com/pqwy/notty.git
    cd notty
    git checkout $version
    dune subst
)

SRC=$TMP/notty

cp -v $SRC/LICENSE.md notty
cp -v $SRC/src/notty.{ml,mli} notty/src
cp -rv $SRC/src/no-uucp notty/src

cp -v $SRC/LICENSE.md notty_unix
cp -v -R $SRC/src-unix/* notty_unix/

git checkout notty/src/dune
git checkout notty_unix/dune

git add -A .
