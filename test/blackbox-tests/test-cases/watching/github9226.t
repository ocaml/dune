  $ . ./helpers.sh

This test demonstrates a bug where multiple rules for .opam are loaded between rebuilds.

  $ cat > dune-project << EOF
  > (lang dune 3.11)
  > (package
  >  (allow_empty)
  >  (name test))
  > (generate_opam_files true)
  > EOF

  $ mkdir src

  $ start_dune
  $ build .
  Success

  $ cat > src/a

  $ build .
  Failure

  $ stop_dune
  Success, waiting for filesystem changes...
  Error: Multiple rules generated for _build/default/test.opam:
  - <internal location>
  - <internal location>
  Had 1 error, waiting for filesystem changes...
