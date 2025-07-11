Exercise the various ways of resolving executable names with `dune exec`.

  $ cat > dune-project << EOF
  > (lang dune 3.20)
  > 
  > (package
  >  (name foo))
  > EOF

  $ cat > dune << EOF
  > (executable
  >  (public_name foo))
  > EOF

  $ cat > foo.ml << EOF
  > let () = print_endline "hello foo"
  > EOF

An executable that would be installed by the current package:
  $ dune describe location foo
  _build/install/default/bin/foo

An executable from the current project:
  $ dune describe location ./foo.exe
  _build/default/foo.exe

Test that executables from dependencies are located correctly:
  $ mkdir dune.lock
  $ cat > dune.lock/lock.dune << EOF
  > (lang package 0.1)
  > EOF
  $ cat > dune.lock/bar.pkg << EOF
  > (version 0.1)
  > (install
  >  (progn
  >   (write-file %{bin}/bar "#!/bin/sh\necho hello bar")
  >   (run chmod a+x %{bin}/bar)))
  > EOF

  $ cat > dune-project << EOF
  > (lang dune 3.20)
  > 
  > (package
  >  (name foo)
  >  (depends bar))
  > EOF

  $ dune describe location bar
  _build/_private/default/.pkg/bar/target/bin/bar

Test that executables from PATH are located correctly:
  $ mkdir bin
  $ cat > bin/baz << EOF
  > #!/bin/sh
  > echo hello baz
  > EOF

  $ chmod a+x bin/baz
  $ export PATH=$PWD/bin:$PATH

  $ dune describe location baz
  $TESTCASE_ROOT/bin/baz

  $ dune exec echo '%{bin:foo}'
  _build/install/default/bin/foo
