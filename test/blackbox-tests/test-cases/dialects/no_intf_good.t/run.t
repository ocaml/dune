Test the (dialect ...) stanza inside the dune-project file.

  $ dune exec ./main.exe

  $ dune build @fmt
  fake ocamlformat is running: "--impl" "fmt.ml"
  Formatting main.mf
  File "fmt.ml", line 1, characters 0-0:
  Error: Files _build/default/fmt.ml and _build/default/.formatted/fmt.ml
  differ.
  [1]
