Selecting an identical lock copy should preserve package reuse, including when
files are shipped alongside the lockfile. No-op actions isolate input staging.

  $ export DUNE_TRACE=+cache
  $ make_dune_project 3.26
  $ make_lockdir
  $ make_lockpkg foo <<EOF
  > (version 1)
  > (build (system "true"))
  > EOF
  $ make_lockpkg_file foo input <<EOF
  > original
  > EOF
  $ cp -R dune.lock other.lock
  $ target=$(get_build_pkg_dir foo)/target
  $ misses() {
  >   dune trace cat | jq -cs --arg target "$target" '
  >     [ .[]
  >       | select(.cat == "cache" and .name == "workspace_local_miss")
  >       | select(.args.target == $target)
  >       | .args.reason | split(":")[0]
  >     ]
  >   '
  > }
  $ dune build "$target"
  $ dune build "$target"
  $ misses
  []

The lock contents are unchanged, but switching to the copy changes the paths
of the adjacent inputs. The rule rebuilds at the same target.

  $ cat > dune-workspace <<EOF
  > (lang dune 3.26)
  > (lock_dir (path other.lock))
  > (context (default (lock_dir other.lock)))
  > EOF
  $ dune build "$target"
  $ misses
  ["rule or dependencies changed"]

Changing an input really must invalidate the rule.

  $ echo updated > other.lock/foo.files/input
  $ dune build "$target"
  $ misses
  ["rule or dependencies changed"]

Without adjacent inputs, switching identical locks reuses the build.

  $ rm -r dune.lock/foo.files other.lock/foo.files
  $ dune build "$target"
  $ rm dune-workspace
  $ dune build "$target"
  $ misses
  []
