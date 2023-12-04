#!/bin/bash

version=1.15.4
name=sha

set -e -o pipefail

TMP="$(mktemp -d)"
trap "rm -rf $TMP" EXIT

rm -rf $name
mkdir -p $name/src

(cd $TMP && opam source $name.$version)

SRC=$TMP/$name.$version

cp -v $SRC/*.{ml,mli,c,h} $name/

git checkout $name/dune
git add -A $name/
