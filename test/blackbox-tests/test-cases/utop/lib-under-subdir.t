dune utop should read libraries in (subdir ..)

  $ make_dune_project 3.6

  $ cat >dune <<EOF
  > (data_only_dirs foo)
  > (subdir foo
  >  (library
  >   (modules foolib)
  >   (name foolib)))
  > EOF

  $ mkdir foo
  $ cat >foo/foolib.ml <<EOF
  > let hw () = print_endline "foolib"
  > EOF

  $ cat >foo.ml <<EOF
  > Foolib.hw ()
  > EOF

  $ dune utop . -- foo.ml
  foolib
