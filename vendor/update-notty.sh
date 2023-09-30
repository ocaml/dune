#!/bin/bash

version=cb7221c73f8009a904fa249fdeb5558c83043b8f

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
