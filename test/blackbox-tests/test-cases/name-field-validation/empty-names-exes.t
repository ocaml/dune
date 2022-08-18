exes: empty list of names/public names
  $ cat >dune-project <<EOF
  > (lang dune 2.0)
  > EOF
  $ cat >dune <<EOF
  > (executables (names))
  > EOF
  $ dune build
  File "dune", line 1, characters 13-20:
  1 | (executables (names))
                   ^^^^^^^
  Error: Not enough arguments for names
  [1]
  $ cat >dune <<EOF
  > (executables (public_names))
  > EOF
  $ dune build
  File "dune", line 1, characters 13-27:
  1 | (executables (public_names))
                   ^^^^^^^^^^^^^^
  Error: Not enough arguments for public_names
  [1]
  $ cat >dune <<EOF
  > (tests (names))
  > EOF
  $ dune build
  File "dune", line 1, characters 7-14:
  1 | (tests (names))
             ^^^^^^^
  Error: Not enough arguments for names
  [1]
