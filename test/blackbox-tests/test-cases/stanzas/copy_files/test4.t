Test external paths:

  $ make_dune_project 2.7
  $ P=$(mktemp)
  $ echo Hola > $P
  $ cat >dune <<EOF
  > (copy_files $P)
  > EOF
  $ dune build $(basename $P)
  $ cat _build/default/$(basename $P)
  Hola
