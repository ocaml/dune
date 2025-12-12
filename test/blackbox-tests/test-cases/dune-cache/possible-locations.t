Showcase all possible locations of the cache.

  $ echo "(lang dune 3.17)" > dune-project

  $ cat > dune << EOF
  > (library
  >  (name foo))
  > EOF

  $ cat > foo.ml << EOF
  > let f x y = x + y
  > EOF

Populate the different cache locations.
- Without any configuration
  $ dune build
  $ dune_cmd exists ~/.cache/dune/db
  true

- With XDG standard config
  $ XDG_CACHE_HOME=$(pwd)/a dune build --force
  $ dune_cmd exists $(pwd)/a/dune/db
  true

- With dune-specific config
  $ DUNE_CACHE_ROOT=$(pwd)/b dune build --force
  $ dune_cmd exists $(pwd)/b/db
  true

- With both of them, only the latter is used
  $ XDG_CACHE_HOME=$(pwd)/c DUNE_CACHE_ROOT=$(pwd)/d dune build --force
  $ dune_cmd exists $(pwd)/c/dune/db
  false
  $ dune_cmd exists $(pwd)/d/db
  true
