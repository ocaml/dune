#!/usr/bin/env bash

version=1.13.2

set -e -o pipefail

TMP="$(mktemp -d)"
trap "rm -rf $TMP" EXIT

rm -rf re
mkdir -p re/src

(
    cd $TMP
    git clone https://github.com/ocaml/ocaml-re.git
    cd ocaml-re
    git checkout $version
)

SRC=$TMP/ocaml-re

cp -v $SRC/LICENSE.md re
cp -v $SRC/lib/*.{ml,mli} re/src/

git add -A .
