#!/usr/bin/env bash

# Script name: build_changelog.sh
# Description: this script updates the CHANGES.md file from the files in
# the changes directory, removing the files as it goes.
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

# Version headers have a format like
#
# ```
# l.n.m. (yyyy-mm-dd)
# -------------------
# ```
#
# Using setext level 2 headings.
# See https://spec.commonmark.org/0.31.2/#setext-headings
generate_version_header() {
  today=$(date "+%Y-%m-%d")

  header="$version ($today)"
  header_size=${#header}
  echo "$header" >> "$output"

  for _ in $(seq "$header_size"); do
    echo -n "-" >> "$output"
  done

  add_newline
  add_newline
}

append_files_in_dir_if_not_empty() {
  subheader="$1"
  category="$2"
  dir="$doc_dir/$category"

  if [ ! -d "$dir" ]; then
    echo "Error: directory $dir doesn't exist."
    exit 1
  fi

  list_of_files=$(find "$dir" -maxdepth 1 -type f -not -name '.*' -name '*.md')

  if [ -z "$list_of_files" ] ; then
    return 0
  fi

  # Subheaders use ATX headings, and must be at level 3 to ensure that they are
  # subordinate to the version headers.
  echo "### $subheader" >> "$output"
  add_newline
  for file in $list_of_files; do
      cat "$file" >> "$output"
      add_newline
      rm "$file"
  done
}

# Move to the Dune root directory
cd "$(dirname "$0")"
cd ../../../

# Create the output file
touch "$output"

# Add the header to the changelog
generate_version_header

# Extract the information from the changes/**/*.md files
append_files_in_dir_if_not_empty "Fixed" "fixed"
append_files_in_dir_if_not_empty "Added" "added"
append_files_in_dir_if_not_empty "Changed" "changed"

# We remove the unreleased header and add the previous changelog to the new
# changelog
tail -n +8 CHANGES.md >> "$output"

# Replace the old changelog with the new one. It ereases the new one.
mv "$output" CHANGES.md
