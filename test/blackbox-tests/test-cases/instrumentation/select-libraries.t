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
