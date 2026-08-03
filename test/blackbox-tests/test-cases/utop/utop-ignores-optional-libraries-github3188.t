This test makes sure that the utop subcommand does not load optional libraries.

  $ mkdir testutop
  $ cat <<EOF > testutop/dune
  > (library
  >  (name testutop)
  >  (optional)
  >  (libraries does_not_exist))
  > EOF
  $ echo 'let run () = print_endline "this will never run"' > testutop/testutop.ml
  $ make_dune_project 2.0
  $ echo 'let () = print_endline "No Error"' > init_test.ml

  $ dune utop testutop -- init_test.ml
  No Error
