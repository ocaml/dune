Allows `dirs` and `data_only_dirs` to coexist.

  $ make_dune_project 1.11
  $ cat >dune <<EOF
  > (dirs foo)
  > (data_only_dirs bar)
  > EOF
  $ mkdir bar && touch bar/x
  $ dune build
