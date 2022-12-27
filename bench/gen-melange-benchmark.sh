#!/bin/bash

set -eu

usage()
{
    cat <<EOF
Usage:
    $(basename "${0}") <command> <clean_command> <name>
EOF
}

if [ $# -ne 3 ]; then
    usage
    exit 1
fi

command="${1}"
clean_command="${2}"
name="${3}"

hyperfine "${command}" \
    --show-output \
    --warmup 1 \
    --runs 3 \
    --prepare "${clean_command}"
    --export-json bench.json \
    > /dev/null

mean_time=$(cat bench.json | jq '.results[0].mean * 1000 | tostring')

cat<<EOF
[
    {
        "name": "${name}",
        "unit": "millisecond",
        "value": ${mean_time}
    }
]
EOF
