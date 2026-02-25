#!/usr/bin/env sh

set -euf -o pipefail

old_errs="$1"
new_errs="$2"

function sort_file () {
    # cat the file, then
    # remove leading/trailing blanks, normalizing fields
    # remove any lines containing only a single field
    cat "$1" \
        | awk '{$1=$1};1' \
        | awk 'NF != 1' \
        | sort \
        | uniq
}

# - Filter out 'failed to build)', because such packages only failed because of an intermediate dependency failure
#   and we only care about the latter.
# - Sort based on the error message: (-k2.3) means start from the second field, 3rd character.
comm -1 -3 <(sort_file "$old_errs") <(sort_file "$new_errs") \
    | grep -v -e 'failed to build)' -e 'failed: Cancelled' -e 'failed: Failed to get sources of' \
    | sort -k2.3 \
    | uniq

# TODO
# awk '{ print $1 }' ~/Scratch/dune.3.21-rc/new-errors.txt > failing-packages.txt
