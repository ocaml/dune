  $ make_instrumentation_backends
  $ make_select_instrumentation_project
  $ exe=./main.exe

The select dependency is ignored when the backend is not active.

  $ dune build "$exe"
  File "main.ml", line 1, characters 23-31:
  1 | let () = print_endline Selected.message
                             ^^^^^^^^
  Error: Unbound module Selected
  [1]

The select is resolved when the backend is active.

  $ dune build --instrument-with hello "$exe"
  $ _build/default/main.exe
  Hello from Dune__exe__Selected!
  Hello from Dune__exe__Main!
  select instrumentation library

The instrumentation libraries are only added once, even when per-module
preprocessing duplicates the instrumentation for each preprocessing
specification.

  $ cat >dune <<'EOF2'
  > (library
  >  (name choice)
  >  (modules choice))
  > 
  > (executable
  >  (name main)
  >  (modes byte)
  >  (modules :standard \ choice)
  >  (preprocess
  >   (per_module
  >    ((pps hello.ppx) other)))
  >  (instrumentation
  >   (backend hello)
  >   (libraries
  >    (select selected.ml from
  >     (choice -> selected.choice.ml)))))
  > EOF2
  $ cat >other.ml <<'EOF2'
  > EOF2
  $ dune build --instrument-with hello "$exe"
  File "dune", lines 15-16, characters 3-63:
  15 |    (select selected.ml from
  16 |     (choice -> selected.choice.ml)))))
  Error: Too many files for module Selected in .:
  - _build/default/selected.ml
  - _build/default/selected.ml
  [1]
