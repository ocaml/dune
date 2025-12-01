Test invalid version numbers in extension declarations. We want to make sure that
such situations provide a clear error.

  $ test_invalid_version() {
  >   cat > dune-project <<EOF
  > (lang dune 3.21)
  > (using menhir $1)
  > EOF
  >   dune build
  > }

Invalid version number:

  $ test_invalid_version "Ali"
  File "dune-project", line 2, characters 14-17:
  2 | (using menhir Ali)
                    ^^^
  Error: Invalid version. Version must be two numbers separated by a dot.
  [1]

Test with various non-ASCII characters:

  $ test_invalid_version "è"
  File "dune-project", line 2, characters 14-16:
  2 | (using menhir è)
                    ^^
  Error: Invalid version. Version must be two numbers separated by a dot.
  Hint: using menhir 3.0
  [1]


  $ test_invalid_version "π3.14"
  File "dune-project", line 2, characters 14-20:
  2 | (using menhir π3.14)
                    ^^^^^^
  Error: Invalid version. Version must be two numbers separated by a dot.
  Hint: using menhir 3.0
  [1]


  $ test_invalid_version "α"
  File "dune-project", line 2, characters 14-16:
  2 | (using menhir α)
                    ^^
  Error: Invalid version. Version must be two numbers separated by a dot.
  Hint: using menhir 3.0
  [1]


  $ test_invalid_version "😀"
  File "dune-project", line 2, characters 14-18:
  2 | (using menhir 😀)
                    ^^^^
  Error: Invalid version. Version must be two numbers separated by a dot.
  Hint: using menhir 3.0
  [1]


  $ test_invalid_version "中3.16文"
  File "dune-project", line 2, characters 14-24:
  2 | (using menhir 中3.16文)
                    ^^^^^^^^^^
  Error: Invalid version. Version must be two numbers separated by a dot.
  Hint: using menhir 3.0
  [1]
