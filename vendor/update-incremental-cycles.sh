#!/bin/bash

version=1e2030a5d5183d84561cde142eecca40e03db2a3

set -e -o pipefail

TMP="$(mktemp -d)"

rm -rf incremental-cycles
mkdir incremental-cycles

(
    cd $TMP
    git clone https://gitlab.inria.fr/agueneau/incremental-cycles.git
    cd incremental-cycles
    git checkout $version
    dune build export/incremental_cycles.ml
)

SRC=$TMP/incremental-cycles

mkdir incremental-cycles/src
cp -v $SRC/LICENSE incremental-cycles
cp -v $SRC/_build/default/export/incremental_cycles.ml incremental-cycles/src/
cp -v $SRC/export/incremental_cycles_intf.ml incremental-cycles/src/
cp -v $SRC/export/incremental_cycles.mli incremental-cycles/src/

git checkout incremental-cycles/src/dune
git add -A .
