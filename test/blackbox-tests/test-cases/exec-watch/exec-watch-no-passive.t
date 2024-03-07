Trying to run exec in passive watch mode produces and error.

  $ cat >dune-project <<EOF
  > (lang dune 3.6)
  > (package (name foo))
  > EOF

  $ cat >dune <<EOF
  > (executable
  >  (public_name foo))
  > EOF

  $ cat >foo.ml <<EOF
  > let () = print_endline "foo"
  > EOF

  $ dune exec --passive-watch-mode ./foo.exe
  Error: passive watch mode is unsupported by exec
  [1]
