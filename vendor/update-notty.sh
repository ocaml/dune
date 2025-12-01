#!/bin/bash

version=54b14f0dc25316a5b64206c55598442ec9d43325

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
