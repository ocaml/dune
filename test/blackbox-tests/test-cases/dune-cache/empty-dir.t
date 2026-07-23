Check the cache restores empty directories

  $ export DUNE_CACHE=enabled
  $ export DUNE_CACHE_ROOT=$PWD/dune-cache
  $ make_empty_child_directory_target_project

Build an empty directory.

  $ dune build output
  $ find _build/default/output | sort
  _build/default/output
  _build/default/output/child
  _build/default/output/file

Restore it from cache.

  $ rm -rf _build
  $ dune build output
  $ find _build/default/output | sort
  _build/default/output
  _build/default/output/child
  _build/default/output/file
