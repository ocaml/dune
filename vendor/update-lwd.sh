#!/bin/bash

version=3c446b45b2d9e81bc72b57ada168fe7923f9b02c

set -e -o pipefail

TMP="$(mktemp -d)"
trap "rm -rf $TMP" EXIT

rm -rf lwd
mkdir -p lwd/lwd lwd/nottui

(
    cd $TMP
    git clone https://github.com/let-def/lwd.git
    cd lwd
    git checkout $version
)

SRC=$TMP/lwd/

cp -v $SRC/LICENSE lwd/
cp -v -R $SRC/lib/lwd/lwd.{ml,mli} lwd/lwd
cp -v -R $SRC/lib/lwd/lwd_utils.{ml,mli} lwd/lwd
cp -v -R $SRC/lib/nottui/nottui.{ml,mli} lwd/nottui


git checkout lwd/{lwd,nottui}/dune
git add -A .
