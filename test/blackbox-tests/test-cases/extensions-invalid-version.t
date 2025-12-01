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

CR-someday benodiwal: Non-ASCII characters in extension versions fail at the
s-expression parsing level, showing a generic "Invalid dune-project file" error
instead of the specific version validation error with hints. This would require
changes to the s-expression parser to handle properly.

  $ test_invalid_version "è"
  File "dune-project", line 2, characters 14-14:
  2 | (using menhir è)
                    
  Error: Invalid dune-project file
  [1]


  $ test_invalid_version "π3.14"
  File "dune-project", line 2, characters 14-14:
  2 | (using menhir π3.14)
                    
  Error: Invalid dune-project file
  [1]


  $ test_invalid_version "α"
  File "dune-project", line 2, characters 14-14:
  2 | (using menhir α)
                    
  Error: Invalid dune-project file
  [1]


  $ test_invalid_version "😀"
  File "dune-project", line 2, characters 14-14:
  2 | (using menhir 😀)
                    
  Error: Invalid dune-project file
  [1]


  $ test_invalid_version "中3.16文"
  File "dune-project", line 2, characters 14-14:
  2 | (using menhir 中3.16文)
                    
  Error: Invalid dune-project file
  [1]

