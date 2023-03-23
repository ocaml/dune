#!/bin/bash

version=5dba1d3c2e1e5e1c6083f62eaed672c85944d7f7

pkg=xxh

set -e -o pipefail

TMP="$(mktemp -d)"
trap "rm -rf $TMP" EXIT

rm -rf $pkg
mkdir -p $pkg

(
    cd $TMP
    git clone https://github.com/rgrinberg/$pkg.git
    cd $pkg
    git checkout $version
)

SRC=$TMP/$pkg

cp -v $SRC/src/*.{ml,mli} $SRC/src/{xxhash.c,xxhash.h,xxh_stubs.c} $pkg/

git checkout $pkg/dune
git add -A .
