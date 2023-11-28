  $ . ./helpers.sh

This test demonstrates a bug where multiple rules for ccomp and .opam are loaded between
rebuilds.

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
