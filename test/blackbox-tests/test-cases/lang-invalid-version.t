Test invalid version numbers in dune-project files. We want to make sure that
such situations provide a clear error.

  $ test_invalid_version() {
  >   cat > dune-project <<EOF
  > (lang dune $1)
  > EOF
  >   dune build
  > }

Invalid version number:

  $ test_invalid_version "Ali"
  File "dune-project", line 1, characters 11-14:
  1 | (lang dune Ali)
                 ^^^
  Error: Atom of the form NNN.NNN expected
  [1]

Test with various non-ASCII characters:

  $ test_invalid_version "è"
  File "dune-project", line 1, characters 11-13:
  1 | (lang dune è)
                 ^^
  Error: Invalid version. Version must be two numbers separated by a dot.
  Hint: try 3.21
  [1]

  $ test_invalid_version "π3.14"
  File "dune-project", line 1, characters 11-17:
  1 | (lang dune π3.14)
                 ^^^^^^
  Error: Invalid version. Version must be two numbers separated by a dot.
  Hint: try 3.21
  [1]

  $ test_invalid_version "α"
  File "dune-project", line 1, characters 11-13:
  1 | (lang dune α)
                 ^^
  Error: Invalid version. Version must be two numbers separated by a dot.
  Hint: try 3.21
  [1]

  $ test_invalid_version "😀"
  File "dune-project", line 1, characters 11-15:
  1 | (lang dune 😀)
                 ^^^^
  Error: Invalid version. Version must be two numbers separated by a dot.
  Hint: try 3.21
  [1]

  $ test_invalid_version "中3.16文"
  File "dune-project", line 1, characters 11-21:
  1 | (lang dune 中3.16文)
                 ^^^^^^^^^^
  Error: Invalid version. Version must be two numbers separated by a dot.
  Hint: try 3.21
  [1]
