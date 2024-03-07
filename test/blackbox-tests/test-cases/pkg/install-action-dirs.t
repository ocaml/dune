Install actions should have the switch directory prepared:

  $ . ./helpers.sh

  $ make_lockdir
  $ cat >dune.lock/test.pkg <<'EOF'
  > (version 0.0.1)
  > (install (system "find %{prefix} | sort"))
  > EOF

  $ build_pkg test
  ../target
  ../target/bin
  ../target/doc
  ../target/doc/test
  ../target/etc
  ../target/etc/test
  ../target/lib
  ../target/lib/stublibs
  ../target/lib/test
  ../target/lib/toplevel
  ../target/man
  ../target/sbin
  ../target/share
  ../target/share/test
