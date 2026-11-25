Reordering supported platforms must not invalidate an unchanged package.
Use no-op build actions and inspect cache misses for its target rule.

  $ export DUNE_TRACE=+cache
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

  $ build_pkg foo
  $ package_target_miss_reasons foo
  ["never seen this target before"]
  $ build_pkg foo
  $ package_target_miss_reasons foo
  []

Only the platform order changes. This does not change the package action or
cause a new target-rule miss.

  $ cat > dune.lock/lock.dune <<EOF
  > (lang package 0.1)
  > (solved_for_platforms ((os macos)) ((os linux)))
  > EOF

  $ build_pkg foo
  $ package_target_miss_reasons foo
  []

The order of alternatives within an explicit platform condition also should
not matter.

  $ make_lockpkg foo <<EOF
  > (version 1)
  > (build
  >  (choice
  >   ((((os linux)) ((os macos))) ((action (system ":"))))))
  > EOF

  $ build_pkg foo
  $ package_target_miss_reasons foo
  ["rule or dependencies changed"]
  $ make_lockpkg foo <<EOF
  > (version 1)
  > (build
  >  (choice
  >   ((((os macos)) ((os linux))) ((action (system ":"))))))
  > EOF

  $ build_pkg foo
  $ package_target_miss_reasons foo
  []
