Test invalid version numbers in dune-project files. We want to make sure that
such situations provide a clear error.

  $ test_invalid_version() {
  >   cat > dune-project <<EOF
  > (lang dune $1)
  > EOF
  >   dune build
  > }

Invalid version number:

CR-someday benodiwal: Consider adding context-specific hints for ASCII invalid
versions (e.g., "Hint: lang dune 3.21"). The current lexer-level approach
trades hints for simplicity and robustness - it works uniformly across all
atoms without needing validation in every decoder.

  $ test_invalid_version "Ali"
  File "dune-project", line 1, characters 11-14:
  1 | (lang dune Ali)
                 ^^^
  Error: Invalid version. Version must be two numbers separated by a dot.
  [1]

Test with various non-ASCII characters:

  $ test_invalid_version "è"
  File "dune-project", line 1, characters 11-12:
  1 | (lang dune è)
                 ^
  Error: Invalid atom: contains non-ASCII character(s). Atoms must only contain
  ASCII characters.
  [1]

  $ test_invalid_version "π3.14"
  File "dune-project", line 1, characters 11-12:
  1 | (lang dune π3.14)
                 ^
  Error: Invalid atom: contains non-ASCII character(s). Atoms must only contain
  ASCII characters.
  [1]

  $ test_invalid_version "α"
  File "dune-project", line 1, characters 11-12:
  1 | (lang dune α)
                 ^
  Error: Invalid atom: contains non-ASCII character(s). Atoms must only contain
  ASCII characters.
  [1]

  $ test_invalid_version "😀"
  File "dune-project", line 1, characters 11-12:
  1 | (lang dune 😀)
                 ^
  Error: Invalid atom: contains non-ASCII character(s). Atoms must only contain
  ASCII characters.
  [1]

  $ test_invalid_version "中3.16文"
  File "dune-project", line 1, characters 11-12:
  1 | (lang dune 中3.16文)
                 ^
  Error: Invalid atom: contains non-ASCII character(s). Atoms must only contain
  ASCII characters.
  [1]
