#!/bin/bash

set -eu

usage()
{
    cat <<EOF
Usage:
    $(basename "${0}") <path_to_dune>
EOF
}

if [ $# -ne 1 ]; then
    usage
    exit 1
fi

path_to_dune="${1}"

start_dune () {
    ((${path_to_dune} build "$@" --watch @all > .#dune-output 2>&1) || (echo exit $? >> .#dune-output)) &
    DUNE_PID=$!;
}

timeout="$(command -v timeout || echo gtimeout)"

with_timeout () {
    $timeout 2 "$@"
    exit_code=$?
    if [ "$exit_code" = 124 ]
    then
        echo Timed out
        cat .#dune-output
    else
        return "$exit_code"
    fi
}

stop_dune () {
    with_timeout dune shutdown;
    wait $DUNE_PID;
    cat .#dune-output;
}

echo Breaking build
echo "let f() = 2" > ./internal/m_1_1_1_1.ml
echo "val f : unit -> int" > ./internal/m_1_1_1_1.mli

echo Starting dune
start_dune

echo Checking for error
until grep 'error' .#dune-output > /dev/null; do sleep 0.1; done

echo Found, fixing build
echo "let f() = ()" > ./internal/m_1_1_1_1.ml
echo "val f : unit -> unit" > ./internal/m_1_1_1_1.mli

echo Checking for success
until grep 'Success' .#dune-output > /dev/null; do sleep 0.1; done

echo Found, stopping dune
stop_dune
