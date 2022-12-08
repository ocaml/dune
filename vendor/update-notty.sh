#!/bin/bash

version=16728c1aa8bebccf6fa064583063ffa8e3074c67

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
