  $ cat > dune-project << EOF
  > (lang dune 3.0)
  > EOF

  $ cat > dune-workspace << EOF
  > (lang dune 3.0)
  > (context
  >  (opam
  >   (switch /absolute/path/to/opam/switchname)))
  > EOF

  $ dune build
  File "dune-workspace", line 4, characters 10-43:
  4 |   (switch /absolute/path/to/opam/switchname)))
                ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
  Error: The name generated from this switch can not be used as a context name.
  Please use (name) to disambiguate.
  Hint: (name switchname) would be a valid context name
  [1]
