#!/bin/bash

# dummy implementation of just enough git to be able to create a lock file

case $1 in
  init)
    # we don't create any repo
    exit 0
    ;;
  ls-remote)
    # hardcoded output, just HEAD pointing to a revision
    echo "058003b274f05b092a391555ffdee8b36f1897b3        HEAD"
    ;;
  fetch)
    # we don't fetch from anywhere
    exit 0
    ;;
  ls-tree)
    # the revision that HEAD is pointing to, just a single empty file
    echo "100644 blob e69de29bb2d1d6434b8b29ae775ad8c2e48c5391       0	empty"
    exit 0
    ;;
  cat-file)
    IFS=':' read -r -a rev_and_path <<< "$3"
    if [[ -z ${rev_and_path[1]} ]]; then
      echo ""
      exit 0
    fi
    if [ "${rev_and_path[1]}" = ".gitmodules" ]; then
      echo "does not exist" >&2
      exit 128
    fi
    echo "Unsupported cat-file command: $@" >&2
    exit 2
    ;;
  *)
    # unsupported, exit out
    echo "Unsupported command: $@" >&2
    exit 2
    ;;
esac
