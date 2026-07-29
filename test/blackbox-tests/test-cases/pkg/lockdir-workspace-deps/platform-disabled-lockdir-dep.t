A lock-dir package can depend on a package that is present in the
lockdir but unavailable for the current platform. Validation accepts
the edge because the name exists somewhere in the lockdir, but rule
generation resolves against the current platform subset and
misclassifies the unavailable lockdir package as a workspace
dependency.

  $ export DUNE_CONFIG__PORTABLE_LOCK_DIR=enabled

  $ make_dune_project 3.24

  $ mkdir -p dune.lock
  $ cat > dune.lock/lock.dune <<EOF
  > (lang package 0.1)
  > (repositories (complete true))
  > (solved_for_platforms ((os linux) (arch x86_64)))
  > EOF

The current test platform is linux/x86_64. Package "inactive" exists
in the lockdir, but is enabled only on macos, while "consumer" depends
on it on all solved platforms.

  $ make_lockpkg inactive <<EOF
  > (version 0.0.1)
  > (enabled_on_platforms (only ((os macos) (arch x86_64))))
  > EOF
  $ make_lockpkg consumer <<'EOF'
  > (version 0.0.1)
  > (depends (all_platforms (inactive)))
  > (build (all_platforms ((action (run echo building-consumer)))))
  > EOF

  $ write_lockdir_consumer_rule

The dependency is rejected as an unavailable lockdir dependency rather
than being treated as a workspace package with an empty install layout.

  $ dune build out
  File "_build/_private/default/.lock/dune.lock/consumer.pkg", line 2, characters 25-33:
  2 | (depends (all_platforms (inactive)))
                               ^^^^^^^^
  Error: The package "consumer" depends on the package "inactive", but
  "inactive" is not available on the current platform.
  [1]
