Test external paths:

  $ cat >dune-project <<EOF
  > (lang dune 2.7)
  > EOF
  $ P=$(mktemp)
  $ echo Hola > $P
  $ cat >dune <<EOF
  > (copy_files $P)
  > EOF
  $ dune build $(basename $P)
  $ cat _build/default/$(basename $P)
  Hola
