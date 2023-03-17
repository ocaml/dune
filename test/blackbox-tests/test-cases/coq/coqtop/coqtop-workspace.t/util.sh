#!/bin/bash

export OUTPUT="$PWD/coqtop_test.tmp"
export PWD0="$PWD"

function coqtop_test() {
  (true | (dune coq top "$1" 1>$OUTPUT 2>&1)) || cat $OUTPUT
}

function check_build() {
  if [[ ! -d "./_build" ]]; then
    echo "No build directory found..."
    exit 1
  fi

  if [[ $(find . -name _build -type d | wc -l) > 1 ]]; then
    echo "More than one _build directory..."
    exit 1
  fi
}
