  $ echo "(lang dune 1.11)" > dune-project
  $ cat >dune <<EOF
  > (dirs foo)
  > (data_only_dirs bar)
  > EOF
  $ mkdir bar && touch bar/x
  $ dune build
