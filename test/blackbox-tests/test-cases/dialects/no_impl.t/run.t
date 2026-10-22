Test the (dialect ...) stanza inside the `dune-project` file.

  $ dune exec ./main.exe

  $ dune build @fmt 2>&1 | grep -v "fake ocamlformat is running"
  Formatting ./main.mfi
  File "fmt.ml", line 1, characters 0-0:
  --- fmt.ml
  +++ fmt.ml.corrected
  @@ -1 +1 @@
  -prerr_endline ("Formatting " ^ Sys.argv.(1))
  +(* fake ocamlformat output *)
  \ No newline at end of file
  File "main.ml", line 1, characters 0-0:
  --- main.ml
  +++ main.ml.corrected
  @@ -1 +1 @@
  -let () = ()
  +(* fake ocamlformat output *)
  \ No newline at end of file
  [1]

  $ dune trace cat | jq_dune -r '
  >   processes
  > | select(.args.prog | basename == "ocamlformat")
  > | .args.stderr
  > | gsub("^\\s+|\\s+$"; "")
  > ' | sort
  fake ocamlformat is running: "--impl" "./fmt.ml"
  fake ocamlformat is running: "--impl" "./main.ml"

Omitting the implementation kind is rejected before Dune 3.9.

  $ rm dune-project
  $ cat > dune-project <<'EOF'
  > (lang dune 3.8)
  > (dialect
  >  (name mlfi)
  >  (interface (extension mfi)))
  > EOF
  $ dune build
  File "dune-project", lines 2-4, characters 0-51:
  2 | (dialect
  3 |  (name mlfi)
  4 |  (interface (extension mfi)))
  Error: omitting (implementation) in dialects is only available since version
  3.9 of the dune language. Please update your dune-project file to have (lang
  dune 3.9).
  [1]
