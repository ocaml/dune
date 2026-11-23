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
  $ linux_digest=$(dune pkg print-digest foo)
  $ build_pkg foo
  $ cat "$pkg_root/$linux_digest/target/platform"
  linux/x86_64

Currently both operating systems have the same package key. The second build
has no target-rule miss and incorrectly reuses the first platform's artifact.

  $ macos_digest=$(DUNE_CONFIG__OS=macos dune pkg print-digest foo)
  $ test "$linux_digest" != "$macos_digest"
  [1]
  $ DUNE_CONFIG__OS=macos dune build "$pkg_root/$macos_digest/target"
  $ package_target_miss_reasons foo
  []
  $ cat "$pkg_root/$macos_digest/target/platform"
  linux/x86_64

Changing architecture has the same problem.

  $ arm_digest=$(DUNE_CONFIG__ARCH=arm64 dune pkg print-digest foo)
  $ test "$linux_digest" != "$arm_digest"
  [1]
  $ DUNE_CONFIG__ARCH=arm64 dune build "$pkg_root/$arm_digest/target"
  $ package_target_miss_reasons foo
  []
  $ cat "$pkg_root/$arm_digest/target/platform"
  linux/x86_64
