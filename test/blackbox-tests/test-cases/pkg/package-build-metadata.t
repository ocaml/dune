Solver preferences and system-package hints do not change a package's build.
Currently they change its key and unnecessarily rerun even a no-op action.

  $ export DUNE_TRACE=+cache
  $ make_dune_project 3.26
  $ make_lockdir
  $ make_lockpkg foo <<EOF
  > (version 1)
  > (build (system "true"))
  > EOF
  $ cp dune.lock/foo.pkg original.pkg
  $ digest=$(dune pkg print-digest foo)
  $ build_pkg foo
  $ build_pkg foo
  $ package_target_miss_reasons foo
  []

Changing only the solver's avoid preference currently invalidates the build.

  $ printf '\n(avoid)\n' >> dune.lock/foo.pkg
  $ test "$digest" = "$(dune pkg print-digest foo)"
  [1]
  $ build_pkg foo
  $ package_target_miss_reasons foo
  ["never seen this target before"]

Changing only a system-package hint also invalidates it.

  $ cp original.pkg dune.lock/foo.pkg
  $ printf '\n(depexts system-package-hint)\n' >> dune.lock/foo.pkg
  $ test "$digest" = "$(dune pkg print-digest foo)"
  [1]
  $ build_pkg foo
  $ package_target_miss_reasons foo
  ["never seen this target before"]
