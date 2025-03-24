Test the (dialect ...) stanza inside the `dune-project` file.

  $ dune exec ./main.exe

  $ dune build @fmt
  fake ocamlformat is running: "--impl" "fmt.ml"
  fake ocamlformat is running: "--impl" "main.ml"
  Formatting main.mfi
  File "fmt.ml", line 1, characters 0-0:
  Error: Files _build/default/fmt.ml and _build/default/.formatted/fmt.ml
  differ.
  File "main.ml", line 1, characters 0-0:
  Error: Files _build/default/main.ml and _build/default/.formatted/main.ml
  differ.
  [1]
