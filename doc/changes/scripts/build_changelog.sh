#!/usr/bin/env bash

# Script name: build_changelog.sh
# Description: this script generates the CHANGES.md file from the changes
# directory. It removes the added files.
# Author(s): The Dune team
# Date: 2025-09-29
#
# Usage: $ ./build_changelog.sh <X.Y.Z> 
# where X.Y.Z is the version of Dune

set -euo pipefail

# Variables
version="$1"
output=".changelog.new"
doc_dir="./doc/changes"

## Functions
add_newline() {
  echo "" >> "$output"
}

generate_version_header() {
  today=$(date "+%Y-%m-%d")

  header="$version ($today)"
  header_size=$(echo "$header" | wc -m)
  echo "$header" >> "$output"

  for i in $(seq $header_size); do
    echo -n "-" >> "$output"
  done

  add_newline
  add_newline
}

cat_files_in_dir_if_not_empty() {
  subheader="$1"
  category="$2"
  dir="$doc_dir/$category"

  if [ ! -d "$dir" ]; then
    echo "Error: directory $dir doesn't exist."
    exit 1
  fi

  list_of_files=$(find "$dir" -maxdepth 1 -type f -not -name ".*")
  number_of_files=$(cat $list_of_files | wc -l)

  if [ "$number_of_files" -ne 0 ]; then
    echo "## $subheader" >> "$output"
    add_newline

    for file in $list_of_files; do
        cat "$file" >> "$output"
        add_newline
        rm "$file"
    done
  fi
}

# Move to the Dune root directory
cd $(dirname $0)
cd ../../../

# Create the output file
touch "$output"

# Add the header to the changelog
generate_version_header

# Extract the information from the changes/**/*.md files
cat_files_in_dir_if_not_empty "Fixed" "fixed"
cat_files_in_dir_if_not_empty "Added" "added"
cat_files_in_dir_if_not_empty "Changed" "changed"

# We remove the unreleased header and add the previous changelog to the new
# changelog
tail -n +8 CHANGES.md >> "$output"

# Replace the old changelog with the new one. It ereases the new one.
mv "$output" CHANGES.md
