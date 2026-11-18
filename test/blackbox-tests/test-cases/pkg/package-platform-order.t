Reordering supported platforms must not invalidate an unchanged package.
Use no-op build actions and inspect cache misses for its target rule.

  $ export DUNE_TRACE=+cache
  $ misses() {
  >   dune trace cat | jq -cs '
  >     [ .[]
  >       | select(.cat == "cache" and .name == "workspace_local_miss")
  >       | select(.args.target | contains("foo.") and endswith("/target"))
  >       | .args.reason | split(":")[0]
  >     ]
  >   '
  > }
  $ make_dune_project 3.26
  $ make_lockdir
  $ cat > dune.lock/lock.dune <<EOF
  > (lang package 0.1)
  > (solved_for_platforms ((os linux)) ((os macos)))
  > EOF
  $ make_lockpkg foo <<EOF
  > (version 1)
  > (build (all_platforms ((action (system "true")))))
  > EOF
  $ digest=$(dune pkg print-digest foo)
  $ build_pkg foo
  $ misses
  ["never seen this target before"]
  $ build_pkg foo
  $ misses
  []

Only the platform order changes. Currently this changes the package key and
causes a new target-rule miss.

  $ cat > dune.lock/lock.dune <<EOF
  > (lang package 0.1)
  > (solved_for_platforms ((os macos)) ((os linux)))
  > EOF
  $ test "$digest" = "$(dune pkg print-digest foo)"
  [1]
  $ build_pkg foo
  $ misses
  ["never seen this target before"]

The order of alternatives within an explicit platform condition also should
not matter.

  $ make_lockpkg foo <<EOF
  > (version 1)
  > (build
  >  (choice
  >   ((((os linux)) ((os macos))) ((action (system ":"))))))
  > EOF
  $ digest=$(dune pkg print-digest foo)
  $ build_pkg foo
  $ misses
  ["never seen this target before"]
  $ make_lockpkg foo <<EOF
  > (version 1)
  > (build
  >  (choice
  >   ((((os macos)) ((os linux))) ((action (system ":"))))))
  > EOF
  $ test "$digest" = "$(dune pkg print-digest foo)"
  [1]
  $ build_pkg foo
  $ misses
  ["never seen this target before"]
