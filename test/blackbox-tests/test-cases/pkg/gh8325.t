Things should be the same whether dependencies are specified or not.

  $ make_lockdir

If we have a package we depend on

  $ mkdir dependency-source
  $ make_lockpkg dependency <<EOF
  > (version 0.0.1)
  > (source (copy $PWD/dependency-source))
  > EOF

And we have a package we want to build

  $ mkdir test-source
  $ make_lockpkg test <<EOF
  > (version 0.0.1)
  > (source (copy $PWD/test-source))
  > (build
  >  (system "command -v cat > /dev/null 2>&1 || echo no cat"))
  > EOF
  $ build_pkg test

It should continue to work even if `dependency` modifies `PATH`:

  $ make_lockpkg test <<EOF
  > (version 0.0.1)
  > (source (copy $PWD/test-source))
  > ; adding deps breaks cat
  > (depends dependency)
  > (build
  >  (system "command -v cat > /dev/null 2>&1 || echo no cat"))
  > EOF
  $ build_pkg test
