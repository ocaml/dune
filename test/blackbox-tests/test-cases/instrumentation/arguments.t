  $ make_instrumentation_backends
  $ make_argument_instrumentation_project

We check that we can pass arguments to the ppx.

  $ dune build --instrument-with hello
  File "dune", line 6, characters 33-39:
  6 |  (instrumentation (backend hello -place Spain)))
                                       ^^^^^^
  Error: The possibility to pass arguments to instrumentation backends is only
  available since version 2.8 of the dune language. Please update your
  dune-project file to have (lang dune 2.8).
  [1]

  $ make_dune_project 2.8
  $ dune build --instrument-with hello
  $ _build/default/main.exe
  Hello from Spain (<none>)!

Check that we do not pass the instrumentation flags when the instrumentation is
disabled. If the flags were passed with the instrumentation disabled, the
following command would fail (as the flags would be passed to the "trivial"
ppx).

  $ dune build

We also check that we can declare dependencies to the ppx.

  $ make_dependency_instrumentation_project 2.8
  $ dune build --instrument-with hello
  File "dune", line 7, characters 65-83:
  7 |  (instrumentation (backend hello -place Spain -file input/input) (deps input/input)))
                                                                       ^^^^^^^^^^^^^^^^^^
  Error: 'deps' is only available since version 2.9 of the dune language.
  Please update your dune-project file to have (lang dune 2.9).
  [1]

  $ make_dune_project 3.0
  $ dune build --instrument-with hello
  $ _build/default/main.exe
  Hello from Spain (really)!

Can also enable with an environment variable.

  $ DUNE_INSTRUMENT_WITH=hello dune build

  $ _build/default/main.exe
  Hello from Spain (really)!
