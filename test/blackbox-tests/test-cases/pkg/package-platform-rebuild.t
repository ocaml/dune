Package build keys should describe the selected platform, not inactive branches
or the lock directory's representation. Record the current extra builds.

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
  $ digest=$(dune pkg print-digest foo)
  $ build_pkg foo
  $ build_pkg foo
  $ package_target_miss_reasons foo
  []

Editing only the macOS command currently changes the Linux package key.

  $ make_lockpkg foo <<EOF
  > (version 1)
  > (build
  >  (choice
  >   ((((os linux))) ((action (system "true"))))
  >   ((((os macos))) ((action (system ":"))))))
  > EOF
  $ test "$digest" = "$(dune pkg print-digest foo)"
  [1]
  $ build_pkg foo
  $ package_target_miss_reasons foo
  ["never seen this target before"]

Adding another supported platform also invalidates the unchanged Linux build.

  $ digest=$(dune pkg print-digest foo)
  $ cat > dune.lock/lock.dune <<EOF
  > (lang package 0.1)
  > (solved_for_platforms ((os linux)) ((os macos)) ((os win32)))
  > EOF
  $ test "$digest" = "$(dune pkg print-digest foo)"
  [1]
  $ build_pkg foo
  $ package_target_miss_reasons foo
  ["never seen this target before"]

An equivalent non-portable lock directory should reuse the build.

  $ digest=$(dune pkg print-digest foo)
  $ make_lockdir
  $ make_lockpkg foo <<EOF
  > (version 1)
  > (build (system "true"))
  > EOF
  $ test "$digest" = "$(dune pkg print-digest foo)"
  [1]
  $ build_pkg foo
  $ package_target_miss_reasons foo
  ["never seen this target before"]

Spelling out an empty dependency list should have no effect either.

  $ digest=$(dune pkg print-digest foo)
  $ printf '\n(depends)\n' >> dune.lock/foo.pkg
  $ test "$digest" = "$(dune pkg print-digest foo)"
  [1]
  $ build_pkg foo
  $ package_target_miss_reasons foo
  ["never seen this target before"]
