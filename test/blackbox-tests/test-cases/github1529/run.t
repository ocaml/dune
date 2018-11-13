Reproduction case for #1529: using an extension when no dune-project
file is present.

  $ dune build
  Info: creating file dune-project with this contents: (lang dune 1.6)
  File "dune", line 1, characters 1-7:
  1 | (menhir (modules parser))
       ^^^^^^
  Error: Unknown constructor menhir
  [1]
