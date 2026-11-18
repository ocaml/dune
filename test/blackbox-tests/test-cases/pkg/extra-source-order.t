Reordering two extra-source declarations must not invalidate a package build.
Use ordinary files and a no-op action to isolate declaration-order changes.

  $ export DUNE_TRACE=+cache
  $ make_dune_project 3.26
  $ make_lockdir
  $ echo first > a
  $ echo second > b
  $ misses() {
  >   dune trace cat | jq -cs '
  >     [ .[]
  >       | select(.cat == "cache" and .name == "workspace_local_miss")
  >       | select(.args.target | contains("foo.") and endswith("/target"))
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
  $ digest=$(dune pkg print-digest foo)
  $ build_pkg foo
  $ build_pkg foo
  $ misses
  []

Only declaration order changes. Currently this changes the package key and
triggers a new target-rule miss despite identical files and build actions.

  $ write_pkg b a
  $ test "$digest" = "$(dune pkg print-digest foo)"
  [1]
  $ build_pkg foo
  $ misses
  ["never seen this target before"]

Changing an input's contents must still invalidate the build.

  $ digest=$(dune pkg print-digest foo)
  $ echo changed > a
  $ test "$digest" = "$(dune pkg print-digest foo)"
  $ build_pkg foo
  $ misses
  ["rule or dependencies changed"]
