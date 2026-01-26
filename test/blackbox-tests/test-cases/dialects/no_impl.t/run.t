Test the (dialect ...) stanza inside the `dune-project` file.

  $ dune exec ./main.exe

  $ dune build @fmt
  fake ocamlformat is running: "--impl" "fmt.ml"
  fake ocamlformat is running: "--impl" "main.ml"
  Formatting main.mfi
  File "fmt.ml", line 1, characters 0-0:
  --- fmt.ml
  +++ .formatted/fmt.ml
  @@ -1 +1 @@
  -prerr_endline ("Formatting " ^ Sys.argv.(1))
  +(* fake ocamlformat output *)
  \ No newline at end of file
  File "main.ml", line 1, characters 0-0:
  --- main.ml
  +++ .formatted/main.ml
  @@ -1 +1 @@
  -let () = ()
  +(* fake ocamlformat output *)
  \ No newline at end of file
  [1]
