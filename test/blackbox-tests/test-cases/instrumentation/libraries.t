  $ make_instrumentation_backends
  $ make_library_instrumentation_project
  $ exe=./main.exe

The libraries field is available starting in Dune 3.25.

  $ cat >dune-project <<'EOF'
  > (lang dune 3.24)
  > EOF
  $ dune build --instrument-with hello "$exe"
  File "dune", line 11, characters 2-20:
  11 |   (libraries helper)))
         ^^^^^^^^^^^^^^^^^^
  Error: 'libraries' is only available since version 3.25 of the dune language.
  Please update your dune-project file to have (lang dune 3.25).
  [1]

  $ cat >dune-project <<'EOF'
  > (lang dune 3.25)
  > EOF

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
