It is forbidden to declare two external implementations with the same variant.

  $ dune build --root 1/
  Entering directory '1'
  Error: Two implementations of vlibfoo have the same variant "somevariant":
  - impl1 (dune:8)
  - impl2 (dune:13)
  [1]

It is forbidden to declare an external implementation and have a local
implementation with the same variant.

  $ dune build --root 2 exe/exe.exe
  Entering directory '2'
  Error: Two implementations of vlibfoo have the same variant "somevariant":
  - impl1 (vlib/dune:8)
  - impl2 (impl/dune:1)
  [1]

  $ dune build --root 3 exe/exe.exe
  Entering directory '3'
  Error: Two implementations of vlibfoo have the same variant "somevariant":
  - impl1 (vlib/dune:9)
  - impl1 (impl/dune:1)
  [1]

  $ dune build --root not-a-vlib
  Entering directory 'not-a-vlib'
  File "dune", line 5, characters 18-24:
  5 |  (virtual_library libfoo)
                        ^^^^^^
  Error: Library libfoo isn't a virtual library.
  [1]

  $ dune build --root public-private
  Entering directory 'public-private'
  Error: Two implementations of libfoo have the same variant "somevariant":
  - impl1 (dune:8)
  - impl2 (dune:13)
  [1]

  $ dune build --root vlib-dont-exist
  Entering directory 'vlib-dont-exist'
  File "dune", line 2, characters 18-30:
  2 |  (virtual_library i-dont-exist)
                        ^^^^^^^^^^^^
  Error: Virtual library i-dont-exist hasn't been found in the project.
  [1]
