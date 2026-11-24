An action can detect its platform without mentioning platform variables in the
lockfile. Its artifacts must not be reused on a different OS or architecture.

  $ export DUNE_TRACE=+cache
  $ make_dune_project 3.26
  $ make_lockdir
  $ make_lockpkg foo <<EOF
  > (version 1)
  > (install
  >  (system "echo \$DUNE_CONFIG__OS/\$DUNE_CONFIG__ARCH > %{prefix}/platform"))
  > EOF
  $ build_pkg foo
  $ cat "$pkg_root/foo/target/platform"
  linux/x86_64

The second build currently has no target-rule miss and incorrectly reuses the
first platform's artifact.

  $ DUNE_CONFIG__OS=macos dune build "$pkg_root/foo/target"
  $ package_target_miss_reasons foo
  []
  $ cat "$pkg_root/foo/target/platform"
  linux/x86_64

Changing architecture has the same problem.

  $ DUNE_CONFIG__ARCH=arm64 dune build "$pkg_root/foo/target"
  $ package_target_miss_reasons foo
  []
  $ cat "$pkg_root/foo/target/platform"
  linux/x86_64
