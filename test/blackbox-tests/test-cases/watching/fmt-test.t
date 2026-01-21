  $ echo '(lang dune 3.20)' > dune-project
  $ cat > dune << EOF
  > (executable
  >  (name foo))
  > EOF

  $ touch .ocamlformat

  $ echo "let ()=print_int (5+4)" > foo.ml

  $ start_dune

  $ dune rpc ping --wait
  Server appears to be responding normally

  $ dune fmt

Remove the fake ocamlformat from the dune file to see the real output
  $ cat foo.ml
  (* fake ocamlformat output *)

  $ stop_dune
  fake ocamlformat is running: "--impl" "foo.ml"
  File "foo.ml", line 1, characters 0-0:
  Error: Files _build/default/foo.ml and _build/default/.formatted/foo.ml
  differ.
  Had 1 error, waiting for filesystem changes...
  Promoting _build/default/.formatted/foo.ml to foo.ml.
