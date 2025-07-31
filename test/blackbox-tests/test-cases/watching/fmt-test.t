
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
  >                          4)
  > 
  > EOF

  $ start_dune

Ultimately we'd want this warning to not appear at all, since `dune fmt` doesn't have arguments
  $ dune fmt 2>&1 | sed 's/pid: [0-9]*/pid: PID/g'
  Warning: Your build request is being forwarded to a running Dune instance
  (pid: PID). Note that certain command line arguments may be ignored.
  Build failed with 1 error:
  File "foo.ml", line 1, characters 0-0:
  Error: Files _build/default/foo.ml and _build/default/.formatted/foo.ml
  differ.
  Promoting _build/default/.formatted/foo.ml to foo.ml.

  $ cat foo.ml
  let () = print_int (5 + 4)

  $ stop_dune
  File "foo.ml", line 1, characters 0-0:
  Error: Files _build/default/foo.ml and _build/default/.formatted/foo.ml
  differ.
  Had 1 error, waiting for filesystem changes...
