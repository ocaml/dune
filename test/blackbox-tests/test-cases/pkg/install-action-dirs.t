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
  find: '../target': No such file or directory
