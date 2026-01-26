#!/bin/sh

version=43b466d43766bb5d75cfa05dce9efe82e4946490
lmdb_version=40d3741b7d40ba4c75cb91dd9987ce692d376d71

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

# Apply Cygwin patch to mdb.c
echo "Applying Cygwin compatibility patch..."
sed -i.bak '/^#ifndef MDB_USE_ROBUST/i\
/* Cygwin does not support robust mutexes */\
#ifdef __CYGWIN__\
# define MDB_USE_ROBUST 0\
#endif\
' ocaml-lmdb/mdb.c && rm ocaml-lmdb/mdb.c.bak

git checkout ocaml-lmdb/dune
git checkout ocaml-lmdb/flags
git add -A .

