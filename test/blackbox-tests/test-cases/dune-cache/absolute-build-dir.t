The shared cache should identify a build directory below the workspace by its
location, regardless of whether it is spelled as a relative or absolute path.

  $ make_dune_project 3.17
  $ cat > dune <<'EOF'
  > (rule
  >  (target target)
  >  (action (with-stdout-to target (echo contents))))
  > EOF
  $ export DUNE_CACHE=enabled
  $ export DUNE_CACHE_ROOT=$PWD/cache
  $ export DUNE_TRACE=cache

Populate the shared cache with the relative build directory.

  $ dune build target
  $ rm -rf _build

Rebuilding with the same directory spelled as an absolute path currently misses
the shared cache. Inspecting cache events tests the lookup directly, without
relying on action output or undeclared targets.

  $ DUNE_BUILD_DIR=$PWD/_build dune build target
  $ dune trace cat \
  > | jq_dune -s '
  >   [ .[]
  >   | select(.cat == "cache")
  >   | select((.args.target // .args.head) | endswith("default/target"))
  >   | .name
  >   ]'
  [
    "workspace_local_miss",
    "miss"
  ]
