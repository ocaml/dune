  $ . ./helpers.sh

This test demonstrates a fixed bug where rules for .install are being duplicated between
rebuilds.

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
  Success

  $ stop_dune
  Success, waiting for filesystem changes...
  Success, waiting for filesystem changes...
