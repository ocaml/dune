#!/bin/bash

version=bb4ab8e99a3ac66d2147d083d38c5213bf482c9f

set -e -o pipefail

TMP="$(mktemp -d)"
trap "rm -rf $TMP" EXIT

rm -rf notty
mkdir -p notty/src

(
    cd $TMP
    git clone https://github.com/ocaml-dune/notty.git
    cd notty
    git checkout $version
)

SRC=$TMP/notty

cp -v $SRC/LICENSE.md notty/
cp -v -R $SRC/{src,src-unix} notty/
rm notty/src/*_top*.ml

git checkout notty/{src,src-unix}/dune
git add -A .
