#!/bin/sh

version=05d9dc328ddf1bd23e946feae16cb5c794a73fac

set -e -o pipefail

TMP="$(mktemp -d)"
trap "rm -rf $TMP" EXIT

rm -rf bigstringaf
mkdir -p bigstringaf

(
    cd $TMP
    git clone https://github.com/inhabitedtype/bigstringaf.git
    cd bigstringaf
    git checkout $version
)

SRC=$TMP/bigstringaf

cp -v $SRC/lib/bigstringaf{.ml,.mli,_stubs.c} bigstringaf
cp -v $SRC/LICENSE bigstringaf/

git checkout bigstringaf/dune
git add -A .

