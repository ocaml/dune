Regression test for #9213: .install rules are not duplicated between rebuilds.

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
