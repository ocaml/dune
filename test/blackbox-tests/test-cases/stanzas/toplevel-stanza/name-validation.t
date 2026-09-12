Custom toplevel names must be filenames.

  $ make_dune_project 3.21

Names containing paths are rejected.

  $ cat >dune <<EOF
  > (toplevel
  >  (name ../foo))
  > EOF

  $ dune build
  File "dune", line 2, characters 7-13:
  2 |  (name ../foo))
             ^^^^^^
  Error: The name field must be a filename without directory components
  [1]

Empty names are also rejected.

  $ cat >dune <<EOF
  > (toplevel
  >  (name ""))
  > EOF

  $ dune build
  File "dune", line 2, characters 7-9:
  2 |  (name ""))
             ^^
  Error: The name field must be a filename without directory components
  [1]
