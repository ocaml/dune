Solver preferences and system-package hints do not change a package's build.
Neither should rerun even a no-op action.

  $ export DUNE_TRACE=+cache
  $ make_dune_project 3.26
  $ make_lockdir
  $ make_lockpkg foo <<EOF
  > (version 1)
  > (build (system "true"))
  > EOF
  $ cp dune.lock/foo.pkg original.pkg

  $ build_pkg foo
  $ build_pkg foo
  $ package_target_miss_reasons foo
  []

Changing only the solver's avoid preference does not invalidate the build.

  $ printf '\n(avoid)\n' >> dune.lock/foo.pkg

  $ build_pkg foo
  $ package_target_miss_reasons foo
  []

Changing only a system-package hint also reuses the build.

  $ cp original.pkg dune.lock/foo.pkg
  $ printf '\n(depexts system-package-hint)\n' >> dune.lock/foo.pkg

  $ build_pkg foo
  $ package_target_miss_reasons foo
  []
