dune utop should read libraries in (subdir ..)

  $ cat >dune-project <<EOF
  > (lang dune 3.6)
  > EOF

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
  File "foo.ml", line 1, characters 0-9:
  1 | Foolib.hw ()
      ^^^^^^^^^
  Error: Unbound module Foolib
  [2]
