#!/bin/bash

version=1bfe079ddfc6bff72a3b6a8ae3c0408297b04434

set -e -u -o pipefail

TMP="$(mktemp -d)"
TRAP_CMD=$(printf "rm -rf %q" "$TMP")
trap "$TRAP_CMD" EXIT

lib_name=ocaml-inotify

rm -rf "$lib_name"
mkdir -p "$lib_name/src"

(
    cd "$TMP"
    git clone https://github.com/ocaml-dune/ocaml-inotify.git "$lib_name"
    cd "$lib_name"
    git -c advice.detachedHead=false checkout "$version"
)

SRC=$TMP/$lib_name

rm "$SRC"/lib/lwt_inotify.ml{,i}
cp -v "$SRC"/lib/*.{ml,mli,c} "$lib_name"/src
cp -v "$SRC"/lib/dune "$lib_name"/src
cp -v "$SRC"/LICENSE.txt "$lib_name"/

git add -A .
