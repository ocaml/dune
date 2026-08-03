Trying to run exec in passive watch mode produces and error.

  $ make_dune_project_with_package 3.6 foo

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
