Regression test for #9225: ccomp and generated .opam rules are not duplicated
between rebuilds.

  $ cat > dune-project << EOF
  > (lang dune 3.11)
  > (package
  >  (allow_empty)
  >  (name test))
  > (generate_opam_files true)
  > EOF

  $ mkdir src

  $ cat > src/dune << EOF
  > (library
  >  (name dune_rpc_lwt_tests)
  >  (foreign_stubs
  >   (language c)
  >   (names stub)))
  > EOF

  $ cat > src/stub.c

  $ start_dune
  $ build .
  Success

  $ cat > src/a

  $ build .
  Success

  $ stop_dune
  Success, waiting for filesystem changes...
  Success, waiting for filesystem changes...
