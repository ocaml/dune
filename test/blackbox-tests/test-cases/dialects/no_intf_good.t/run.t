Test the (dialect ...) stanza inside the dune-project file.

  $ dune exec ./main.exe

  $ dune build @fmt
  fake ocamlformat is running: "--impl" "fmt.ml"
  Formatting main.mf
  File "fmt.ml", line 1, characters 0-0:
  --- fmt.ml
  +++ .formatted/fmt.ml
  @@ -1 +1 @@
  -prerr_endline ("Formatting " ^ Sys.argv.(1))
  +(* fake ocamlformat output *)
  \ No newline at end of file
  [1]
