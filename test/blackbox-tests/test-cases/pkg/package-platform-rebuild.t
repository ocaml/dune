Package build keys should describe the selected platform, not inactive branches
or the lock directory's representation.

  $ export DUNE_TRACE=+cache
  $ make_dune_project 3.26
  $ make_lockdir
  $ cat > dune.lock/lock.dune <<EOF
  > (lang package 0.1)
  > (solved_for_platforms ((os linux)) ((os macos)))
  > EOF
  $ make_lockpkg foo <<EOF
  > (version 1)
  > (build
  >  (choice
  >   ((((os linux))) ((action (system "true"))))
  >   ((((os macos))) ((action (system "false"))))))
  > EOF

  $ build_pkg foo
  $ build_pkg foo
  $ package_target_miss_reasons foo
  []

Editing only the macOS command does not invalidate the Linux package.

  $ make_lockpkg foo <<EOF
  > (version 1)
  > (build
  >  (choice
  >   ((((os linux))) ((action (system "true"))))
  >   ((((os macos))) ((action (system ":"))))))
  > EOF

  $ build_pkg foo
  $ package_target_miss_reasons foo
  []

Adding another supported platform also preserves the unchanged Linux build.

  $ cat > dune.lock/lock.dune <<EOF
  > (lang package 0.1)
  > (solved_for_platforms ((os linux)) ((os macos)) ((os win32)))
  > EOF

  $ build_pkg foo
  $ package_target_miss_reasons foo
  []

An equivalent non-portable lock directory should reuse the build.

  $ make_lockdir
  $ make_lockpkg foo <<EOF
  > (version 1)
  > (build (system "true"))
  > EOF

  $ build_pkg foo
  $ package_target_miss_reasons foo
  []

Spelling out an empty dependency list should have no effect either.

  $ printf '\n(depends)\n' >> dune.lock/foo.pkg

  $ build_pkg foo
  $ package_target_miss_reasons foo
  []
