  $ . ./helpers.sh

  $ echo '(lang dune 3.20)' > dune-project
  $ cat > dune << EOF
  > (executable
  >  (name foo))
  > EOF

  $ touch .ocamlformat

  $ cat > foo.ml << EOF
  > let () =
  >  print_int
  >                         (5
  >       +
  >                            4)
  > 
  > EOF

  $ start_dune

  $ dune rpc ping --wait
  Server appears to be responding normally

  $ dune fmt

  $ cat foo.ml
  let () = print_int (5 + 4)

  $ stop_dune
  File "foo.ml", line 1, characters 0-0:
  Error: Files _build/default/foo.ml and _build/default/.formatted/foo.ml
  differ.
  Had 1 error, waiting for filesystem changes...
  Promoting _build/default/.formatted/foo.ml to foo.ml.
