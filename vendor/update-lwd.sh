#!/usr/bin/env bash
set -euo pipefail

version=cba75b9b71afd15b1f80c507921b96f44495f3f4

set -e -o pipefail

TMP="$(mktemp -d)"
trap "rm -rf $TMP" EXIT

rm -rf lwd
mkdir -p lwd

(
    cd $TMP
    git clone https://github.com/let-def/lwd.git
    cd lwd
    git checkout $version
)

SRC=$TMP/lwd

cp -v $SRC/lib/lwd/* lwd/
mv lwd/lwd_infix_letop.ml lwd/lwd_infx.ml
mv lwd/lwd_infix_letop.mli lwd/lwd_infx.mli

git checkout lwd/dune
git add -A .
