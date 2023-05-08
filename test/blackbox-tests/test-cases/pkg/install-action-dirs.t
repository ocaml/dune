Install actions should have the switch directory prepared:

  $ mkdir dune.lock
  $ cat >dune.lock/lock.dune <<EOF
  > (lang package 0.1)
  > EOF
  $ cat >dune.lock/test <<'EOF'
  > (build (run true))
  > (install (system "find %{prefix} | sort"))
  > EOF

  $ dune build .pkg/test/target/
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
