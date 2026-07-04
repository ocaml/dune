#!/usr/bin/env bash

set -euo pipefail

errors=0

report_error() {
  echo "$1" >&2
  errors=$((errors + 1))
}

check_entry() {
  local entry="$1"
  local name="${entry##*/}"

  if [[ ! "$name" =~ ^[[:digit:]]+\.md$ ]]; then
    report_error \
      "$entry: expected filename to be a PR number followed by .md"
    return
  fi

  if [[ ! -f "$entry" ]]; then
    report_error "$entry: expected a regular file"
    return
  fi

  local pr="${name%.md}"
  local first_line
  first_line="$(head -n 1 "$entry")"

  if [[ "$first_line" != -* ]]; then
    report_error "$entry: first line must start with '-'"
  fi

  local contents
  contents="$(< "$entry")"
  local pr_then_user="\\([^)]*#${pr}([^[:digit:]][^)]*@|@)[^)]*\\)"
  local user_then_pr="\\([^)]*@[^)]*#${pr}([^[:digit:]][^)]*)?\\)"

  if [[ ! $contents =~ $pr_then_user && ! $contents =~ $user_then_pr ]]; then
    report_error \
      "$entry: expected parenthesized reference with #$pr and @username"
  fi
}

check_dir() {
  local dir="$1"

  if [[ ! -d "$dir" ]]; then
    report_error "$dir: directory does not exist"
    return
  fi

  local entry
  while IFS= read -r -d '' entry; do
    check_entry "$entry"
  done < <(find "$dir" -mindepth 1 -maxdepth 1 ! -name '.*' -print0 | sort -z)
}

if [[ $# -eq 0 ]]; then
  cd "$(dirname "$0")"
  set -- added changed fixed
fi

for dir in "$@"; do
  check_dir "$dir"
done

if ((errors > 0)); then
  exit 1
fi
