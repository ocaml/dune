It is forbidden to declare two external implementations with the same variant.

  $ dune build --root 1/
  Entering directory '1'
  Error: Two implementations of vlibfoo have the same variant "somevariant":
  - impl1 (dune:5)
  - impl2 (dune:10)
  [1]

It is forbidden to declare an external implementation and have a local
implementation with the same variant.

  $ dune build --root 2 exe/exe.exe
  Entering directory '2'
  Error: Two implementations of vlibfoo have the same variant "somevariant":
  - impl1 (vlib/dune:5)
  - impl2 (impl/dune:1)
  [1]

  $ dune build --root 3 exe/exe.exe
  Entering directory '3'
  Error: Two implementations of vlibfoo have the same variant "somevariant":
  - impl1 (vlib/dune:6)
  - impl1 (impl/dune:1)
  [1]

  $ dune build --root not-a-vlib
  Entering directory 'not-a-vlib'
  File "dune", line 4, characters 0-91:
  4 | (external_variant
  5 |  (virtual_library libfoo)
  6 |  (variant somevariant)
  7 |  (implementation impl1))
  Error: Library libfoo isn't a virtual library.
  [1]

  $ dune build --root public-private
  Entering directory 'public-private'
  Error: Two implementations of libfoo have the same variant "somevariant":
  - impl1 (dune:5)
  - impl2 (dune:10)
  [1]

  $ dune build --root vlib-dont-exist
  Entering directory 'vlib-dont-exist'
  File "dune", line 1, characters 0-97:
  1 | (external_variant
  2 |  (virtual_library i-dont-exist)
  3 |  (variant somevariant)
  4 |  (implementation impl1))
  Error: Virtual library i-dont-exist hasn't been found in the project.
  [1]
