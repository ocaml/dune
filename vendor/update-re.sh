#!/bin/bash

version=1.11.0

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

echo "include Re" > re/src/dune_re.ml
cat >re/src/dune <<EOF
(library
 (name dune_re)
 (public_name dune-private-libs.dune_re)
 (synopsis "Internal Dune library, do not use!"))
EOF

git add -A .
