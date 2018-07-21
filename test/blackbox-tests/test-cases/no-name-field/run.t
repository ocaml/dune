the name field can be omitted for libraries when public_name is present
  $ dune build --root no-name-lib
  File "dune-project", line 1, characters 11-14:
  Error: Version 1.1 of dune is not supported.
  Supported versions:
  - 0.0
  - 1.0
  [1]

this isn't possible for older syntax <= (1, 0)
  $ dune build --root no-name-lib-syntax-1-0
  File "dune", line 1, characters 9-26:
  Error: You cannot declare items to be installed without adding a <package>.opam file at the root of your project.
  To declare elements to be installed as part of package "foo", add a "foo.opam" file at the root of your project.
  [1]

executable(s) stanza works the same way

  $ dune build --root no-name-exes
  File "dune-project", line 1, characters 11-14:
  Error: Version 1.1 of dune is not supported.
  Supported versions:
  - 0.0
  - 1.0
  [1]

  $ dune build --root no-name-exes-syntax-1-0
  File "dune-project", line 1, characters 0-15:
  Error: Invalid first line, expected: (lang <lang> <version>)
  [1]
