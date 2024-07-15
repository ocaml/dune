
  $ cat > dune-project << EOF
  > (lang dune 3.0)
  > EOF

  $ cat > dune-workspace << EOF
  > (lang dune 3.0)
  > (context
  >  (opam
  >   (switch /absolute/path/to/opam/switch)))
  > EOF

  $ dune build
  File "dune-workspace", line 4, characters 10-39:
  4 |   (switch /absolute/path/to/opam/switch)))
                ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
  Error: Generated context name "/absolute/path/to/opam/switch" is invalid
  Please specify a context name manually with the (name ..) field
  [1]
