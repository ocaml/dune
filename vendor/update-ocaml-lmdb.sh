#!/bin/sh

version=6bcf6fe35167efc44b15bf14a909fb2fd53de923
lmdb_version=14d6629bc8a9fe40d8a6bee1bf71c45afe7576b6

set -e -o pipefail

TMP="$(mktemp -d)"
trap "rm -rf $TMP" EXIT

rm -rf ocaml-lmdb
mkdir -p ocaml-lmdb

(
    cd $TMP
    git clone https://github.com/Drup/ocaml-lmdb.git
    cd ocaml-lmdb
    git checkout $version
)

(
    cd $TMP
    git clone https://github.com/LMDB/lmdb.git
    cd lmdb
    git checkout $lmdb_version
)

SRC=$TMP/ocaml-lmdb
LMDB_SRC=$TMP/lmdb/libraries/liblmdb

cp -v $SRC/src/lmdb.{ml,mli} ocaml-lmdb/
cp -v $SRC/src/lmdb_bindings.{ml,mli} ocaml-lmdb/
cp -v $SRC/src/lmdb_stubs.c ocaml-lmdb/
cp -v $SRC/LICENSE.md ocaml-lmdb/

cp -v $LMDB_SRC/lmdb.h ocaml-lmdb/
cp -v $LMDB_SRC/mdb.c ocaml-lmdb/
cp -v $LMDB_SRC/midl.{c,h} ocaml-lmdb/
cp -v $LMDB_SRC/COPYRIGHT ocaml-lmdb/
cp -v $LMDB_SRC/LICENSE ocaml-lmdb/LICENSE-lmdb

git checkout ocaml-lmdb/dune
git add -A .

