  $ . ./helpers.sh

This test demonstrates a bug where rules are being duplicated between rebuilds.

  $ cat > dune-project << EOF
  > (lang dune 3.11)
  > (package
  >  (allow_empty)
  >  (name test))
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
  Error: Multiple rules generated for _build/default/test.install:
  - <internal location>
  - <internal location>
  Had 1 error, waiting for filesystem changes...
