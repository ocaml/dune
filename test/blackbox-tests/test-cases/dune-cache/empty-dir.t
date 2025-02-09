Check the cache restores empty directories

  $ export DUNE_CACHE=enabled
  $ export DUNE_CACHE_ROOT=$PWD/dune-cache
  $ cat >dune-project <<EOF
  > (lang dune 3.10)
  > (using directory-targets 0.1)
  > EOF

  $ cat >dune <<EOF
  > (rule
  >  (target (dir output))
  >  (action
  >   (progn
  >    (run mkdir output)
  >    (run mkdir output/child)
  >    (run touch output/file))))
  > EOF

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
