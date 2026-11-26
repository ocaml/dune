Install actions should have the switch directory prepared:

  $ make_lockdir
  $ make_lockpkg test <<EOF
  > (version 0.0.1)
  > (install (system "find %{prefix} | sort"))
  > EOF

  $ build_pkg test
  ./_install
  ./_install/bin
  ./_install/doc
  ./_install/doc/test
  ./_install/etc
  ./_install/etc/test
  ./_install/lib
  ./_install/lib/stublibs
  ./_install/lib/test
  ./_install/lib/toplevel
  ./_install/man
  ./_install/sbin
  ./_install/share
  ./_install/share/test
