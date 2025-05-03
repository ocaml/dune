#!/bin/sh

version=9e836575a82f8307baae44243eb168d9943c3a58

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
    cp -v $src/vendor/*.{ml,mli,c,h} $pkg/
) || true

git checkout $pkg/dune

git add -A .
