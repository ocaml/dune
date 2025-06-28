#!/bin/sh

version=c6aa40e5f1973c2e6b736660ce2c8dcd3b3f9c9f

set -e -o pipefail

TMP="$(mktemp -d)"
trap "rm -rf $TMP" EXIT

pkg=ocaml-blake3-mini

rm -rf $pkg
mkdir -p $pkg/{vendor,src}

(
    cd $TMP
    git clone https://github.com/rgrinberg/$pkg.git
    cd $pkg
    git checkout $version
)

src=$TMP/$pkg

(
    cp -v $src/src/*.{ml,mli,c} $pkg/
    cp -v $src/vendor/*.{ml,mli,c,h,S,asm} $pkg/
    rm $pkg/blake3_avx2.c
    rm $pkg/blake3_avx512.c
    rm $pkg/blake3_sse2.c
    rm $pkg/blake3_sse41.c
) || true

git checkout $pkg/dune

git add -A .
