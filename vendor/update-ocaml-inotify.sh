#!/bin/bash

version=469b2c2c3797ce3d9b164bdfddde644c2506793f

set -e -u -o pipefail

TMP="$(mktemp -d)"
TRAP_CMD=$(printf "rm -rf %q" "$TMP")
trap "$TRAP_CMD" EXIT

lib_name=ocaml-inotify

rm -rf "$lib_name"
mkdir -p "$lib_name/src"

(
    cd "$TMP"
    git clone https://github.com/whitequark/ocaml-inotify.git "$lib_name"
    cd "$lib_name"
    git checkout "$version"
)

SRC=$TMP/$lib_name

rm "$SRC"/lib/lwt_inotify.ml{,i}
cp -v "$SRC"/lib/*.{ml,mli,c} "$lib_name"/src

git checkout HEAD "$lib_name"/src/dune || true
git add -A .
