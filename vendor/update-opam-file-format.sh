#!/bin/bash

version=426a63dadaba7f47435c9b01d8cb932c548ff2e2
name=opam-file-format

set -e -o pipefail

TMP="$(mktemp -d)"
trap "rm -rf $TMP" EXIT

rm -rf $name
mkdir -p $name/

(
    cd $TMP
    git clone https://github.com/ocaml-dune/$name.git
    cd $name
    git checkout $version
)

SRC=$TMP/$name

cp -v $SRC/src/*.{ml,mli,mll,mly} $SRC/LICENSE $name/

git checkout $name/dune
git add -A .
