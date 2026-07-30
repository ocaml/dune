  $ make_instrumentation_backends
  $ make_library_instrumentation_project
  $ exe=./main.exe

The instrumentation library is ignored when the backend is not active.

  $ dune build "$exe"
  File "main.ml", line 1, characters 23-29:
  1 | let () = print_endline Helper.message
                             ^^^^^^
  Error: Unbound module Helper
  [1]

The library is added to the executable dependencies when the backend is active.

  $ dune build --instrument-with hello "$exe"
  $ _build/default/main.exe
  Hello from Dune__exe__Main!
  instrumentation library
