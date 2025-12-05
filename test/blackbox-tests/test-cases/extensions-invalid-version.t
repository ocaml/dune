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

CR-someday benodiwal: Consider adding context-specific hints for ASCII invalid
versions (e.g., "Hint: using menhir 3.0"). The current lexer-level approach
trades hints for simplicity and robustness - it works uniformly across all
atoms without needing validation in every decoder.

  $ test_invalid_version "Ali"
  File "dune-project", line 2, characters 14-17:
  2 | (using menhir Ali)
                    ^^^
  Error: Invalid version. Version must be two numbers separated by a dot.
  [1]

Test with various non-ASCII characters:

  $ test_invalid_version "è"
  File "dune-project", line 2, characters 14-15:
  2 | (using menhir è)
                    ^
  Error: Invalid atom: contains non-ASCII character(s). Atoms must only contain
  ASCII characters.
  [1]


  $ test_invalid_version "π3.14"
  File "dune-project", line 2, characters 14-15:
  2 | (using menhir π3.14)
                    ^
  Error: Invalid atom: contains non-ASCII character(s). Atoms must only contain
  ASCII characters.
  [1]


  $ test_invalid_version "α"
  File "dune-project", line 2, characters 14-15:
  2 | (using menhir α)
                    ^
  Error: Invalid atom: contains non-ASCII character(s). Atoms must only contain
  ASCII characters.
  [1]


  $ test_invalid_version "😀"
  File "dune-project", line 2, characters 14-15:
  2 | (using menhir 😀)
                    ^
  Error: Invalid atom: contains non-ASCII character(s). Atoms must only contain
  ASCII characters.
  [1]

  $ test_invalid_version "中3.16文"
  File "dune-project", line 2, characters 14-15:
  2 | (using menhir 中3.16文)
                    ^
  Error: Invalid atom: contains non-ASCII character(s). Atoms must only contain
  ASCII characters.
  [1]

Test with multiple extensions:

  $ cat > dune-project <<EOF
  > (lang dune 3.21)
  > (using menhir 3.0)
  > (using melange 0.1)
  > EOF
  $ dune build

Multiple extensions with one invalid (ASCII):

  $ cat > dune-project <<EOF
  > (lang dune 3.21)
  > (using menhir 3.0)
  > (using melange invalid)
  > EOF
  $ dune build
  File "dune-project", line 3, characters 15-22:
  3 | (using melange invalid)
                     ^^^^^^^
  Error: Invalid version. Version must be two numbers separated by a dot.
  [1]

Multiple extensions with one invalid (non-ASCII):

  $ cat > dune-project <<EOF
  > (lang dune 3.21)
  > (using menhir 3.0)
  > (using melange 😀)
  > EOF
  $ dune build
  File "dune-project", line 3, characters 15-16:
  3 | (using melange 😀)
                     ^
  Error: Invalid atom: contains non-ASCII character(s). Atoms must only contain
  ASCII characters.
  [1]

Multiple extensions with first one invalid:

  $ cat > dune-project <<EOF
  > (lang dune 3.21)
  > (using menhir bad)
  > (using melange 0.1)
  > EOF
  $ dune build
  File "dune-project", line 2, characters 14-17:
  2 | (using menhir bad)
                    ^^^
  Error: Invalid version. Version must be two numbers separated by a dot.
  [1]

Multiple extensions both invalid:

  $ cat > dune-project <<EOF
  > (lang dune 3.21)
  > (using menhir abc)
  > (using melange xyz)
  > EOF
  $ dune build
  File "dune-project", line 3, characters 15-18:
  3 | (using melange xyz)
                     ^^^
  Error: Invalid version. Version must be two numbers separated by a dot.
  [1]
