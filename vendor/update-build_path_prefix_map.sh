#!/bin/bash

version=ce7ae8f59be82120d959ce01582bab3d0bcdebcf

set -e -o pipefail

TMP="$(mktemp -d)"
trap "rm -rf $TMP" EXIT

rm -rf build_path_prefix_map
mkdir -p build_path_prefix_map/src

(
    cd $TMP
    git clone https://gitlab.com/gasche/build_path_prefix_map.git
    cd build_path_prefix_map
    git checkout $version
)

SRC=$TMP/build_path_prefix_map

cp -v $SRC/build_path_prefix_map.{ml,mli} build_path_prefix_map/src

git checkout build_path_prefix_map/src/dune
git add -A .
