The misc install section isn't supported:

  $ cat >dune-project <<EOF
  > (lang dune 3.7)
  > (package
  >  (name xxx))
  > EOF

  $ cat >dune <<EOF
  > (install
  >  (section misc)
  >  (files foo))
  > EOF

  $ dune build xxx.install
  File "dune", line 2, characters 10-14:
  2 |  (section misc)
                ^^^^
  Error: The misc section is not supported by install stanzas.
  [1]
