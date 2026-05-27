The dune cache should be enabled by default

  $ echo "(lang dune 3.17)" > dune-project

  $ cat > dune << EOF
  > (library
  >  (name foo))
  > EOF

  $ cat > foo.ml << EOF
  > let f x y = x + y
  > EOF

Set up cache directory

  $ export DUNE_CACHE_ROOT=$(pwd)/dune_test_cache
  $ mkdir $DUNE_CACHE_ROOT
  $ DUNE_CACHE=disabled dune build
  $ ls $DUNE_CACHE_ROOT

We have not written anything to the cache yet.

Change source files to force a recompilation

  $ cat > foo.ml << EOF
  > let f x y = x - y
  > EOF
  $ dune build
  $ ls $DUNE_CACHE_ROOT/db | sort
  files
  meta
  temp

Cache has been written to!

User rules are not cached by default. They only use the shared cache when it is
explicitly enabled, rather than in the default enabled-except-user-rules mode.

  $ rm -rf _build $DUNE_CACHE_ROOT
  $ cat > dune <<EOF
  > (rule
  >  (target target)
  >  (action
  >   (progn
  >    (no-infer (with-stdout-to marker (echo rebuilt)))
  >    (with-stdout-to target (echo user-rule)))))
  > EOF

In the default mode, deleting [_build] causes the rule to run again.

  $ dune build target
  $ dune_cmd exists _build/default/marker
  true
  $ rm -rf _build
  $ dune build target
  $ dune_cmd exists _build/default/marker
  true

With [--cache=enabled], deleting [_build] restores the target from the shared
cache instead.

  $ rm -rf _build $DUNE_CACHE_ROOT
  $ dune build --cache=enabled target
  $ dune_cmd exists _build/default/marker
  true
  $ rm -rf _build
  $ dune build --cache=enabled target
  $ dune_cmd exists _build/default/marker
  false
