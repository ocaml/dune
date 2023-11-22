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
  Failure

  $ stop_dune
  Success, waiting for filesystem changes...
  Error: Multiple rules generated for _build/default/.dune/ccomp/ccomp:
  - <internal location>
  - <internal location>
  -> required by _build/default/src/stub.o
  -> required by _build/default/src/dlldune_rpc_lwt_tests_stubs.so
  -> required by alias src/all
  -> required by alias src/default
  Error: Multiple rules generated for _build/default/test.opam:
  - <internal location>
  - <internal location>
  Had 2 errors, waiting for filesystem changes...
