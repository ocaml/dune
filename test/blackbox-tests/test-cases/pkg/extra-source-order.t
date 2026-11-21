Reordering two extra-source declarations must not invalidate a package build.
Use ordinary files and a no-op action to isolate declaration-order changes.

  $ export DUNE_TRACE=+cache
  $ make_dune_project 3.26
  $ make_lockdir
  $ echo first > a
  $ echo second > b
  $ misses() {
  >   dune trace cat | jq -cs --arg target "$target" '
  >     [ .[]
  >       | select(.cat == "cache" and .name == "workspace_local_miss")
  >       | select(.args.target == $target)
  >       | .args.reason | split(":")[0]
  >     ]
  >   '
  > }
  $ write_pkg() {
  >   make_lockpkg foo <<EOF
  > (version 1)
  > (extra_sources ($1 (copy $PWD/$1)) ($2 (copy $PWD/$2)))
  > (build (system "true"))
  > EOF
  > }
  $ write_pkg a b
  $ target=$(get_build_pkg_dir foo)/target
  $ dune build "$target"
  $ dune build "$target"
  $ misses
  []

Change only the declaration order. Compare the target path and check whether
Dune reuses the previous build.

  $ previous=$target
  $ write_pkg b a
  $ target=$(get_build_pkg_dir foo)/target
  $ test "$previous" = "$target"
  [1]
  $ dune build "$target"
  $ misses
  ["never seen this target before"]

Changing an input's contents must still invalidate the build.

  $ echo changed > a
  $ test "$target" = "$(get_build_pkg_dir foo)/target"
  $ dune build "$target"
  $ misses
  ["rule or dependencies changed"]
